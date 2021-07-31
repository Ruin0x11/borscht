#![feature(entry_insert)]

extern crate serde;
extern crate serde_derive;
extern crate ron;
extern crate sha2;
extern crate anyhow;
extern crate exe2ax;

use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::Write;
use std::fmt;
use anyhow::{anyhow, Result};
use serde_derive::{Serialize, Deserialize};
use exe2ax::as_::ax3::*;
use exe2ax::as_::ax3::ast;
use exe2ax::as_::dictionary::HspDictionaryValue;
use exe2ax::as_::ax3::lexical::*;
use exe2ax::as_::ax3::visitor::{self, *};
use sha2::{Digest, Sha256};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum DiagnosticKind {
    Info,
    Warning,
    Error
}

impl fmt::Display for DiagnosticKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            &DiagnosticKind::Info => "info",
            &DiagnosticKind::Warning => "warning",
            &DiagnosticKind::Error => "error",
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Diagnostic {
    kind: DiagnosticKind,
    msg: String
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.msg)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Diagnostics {
            diagnostics: Vec::new()
        }
    }

    pub fn push(&mut self, kind: DiagnosticKind, msg: String) {
        self.diagnostics.push(Diagnostic {
            kind,
            msg,
        })
    }

    pub fn iter(&self) -> impl Iterator<Item=&Diagnostic> {
        self.diagnostics.iter()
    }
}

pub struct AnalysisOptions {
    pub db_name: String
}

pub struct AnalysisResult {
    pub node: ast::AstNode,
    pub files: HashMap<String, ast::AstNode>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Metadata {
    variant_name: String,
    version: String,
    ax_sha256: String
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct AnalysisConfig {
    meta: Metadata,
    #[serde(default)]
    includes: Vec<String>,
    variable_groups: HashMap<GroupName, VariableGroup>,
    arrays: HashMap<String, ArrayDefinition>,
    expressions: HashMap<String, ExprDefinition>,
    functions: HashMap<String, FunctionDefinition>,
    labels: HashMap<String, LabelDefinition>,
    files: HashMap<String, FileDefinition>
}

type GroupName = String;

#[derive(Clone, Debug, Serialize, Deserialize)]
struct VariableGroup {
    #[serde(default)]
    includes: HashSet<GroupName>,

    // This has to be a Vec to preserve source code output order.
    #[serde(default)]
    variables: Vec<VariableDefinition>,

    #[serde(default)]
    ignore: HashSet<ron::Value>,

    #[serde(skip_serializing, default)]
    _resolved_variables: Vec<VariableDefinition>,
}

impl VariableGroup {
    fn iter_resolved_variables<'a>(&'a self) -> impl Iterator<Item = &'a VariableDefinition> {
        self._resolved_variables.iter().filter(|v| !v.exclude)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct VariableDefinition {
    /// Name of the variable to be used in the replacement. (`CDATA_ID`)
    name: String,

    /// Constant literal value to be searched for in the code, usually an
    /// integer. (`27`)
    value: ron::Value,

    /// Source code to output when writing the definition of the variable to a
    /// file.
    ///
    /// This is used for constants like `252`, which should actually resolve to
    /// `GDATA_FLAG_VISITED_LARNA = (STARTING_GDATA_FLAG + 2)` in the source
    /// code.
    ///
    /// The two expressions must be equivalent in HSP, or it is programmer
    /// error.
    #[serde(default)]
    code_value: Option<String>,

    /// Exclude this variant from being considered in group rules or
    /// substitutions, but still output it when the source code is written.
    ///
    /// Note that if a set of variable names is used, like in the
    /// VariantRecursive rule, this flag will be ignored, since the variant was
    /// explicitly asked for.
    #[serde(default)]
    exclude: bool,

    /// If true, and there is more than one variable with the same value, this
    /// variable will be used instead of outputting an ambiguous variable
    /// comment.
    ///
    /// There can only be at most one variable with `primary` set to true among
    /// variables with the same value.
    #[serde(default)]
    primary: bool
}

type ArrayRules = HashMap<usize, Rule>;
type ArraySubstitutes = HashMap<usize, Substitute>;

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Rule {
    /// Matches anything.
    Any,

    /// Matches a constant or named variant in a group.
    ///
    /// `Group("cdata")` matches `32` and the variable name
    /// `CDATA_ATTACK_STYLE`.
    Group(String),

    /// Matches an integer constant.
    Constant(i32),

    /// Matches if any of the given variants in the group are found.
    ///
    /// `Variant("cdata", ["CDATA_PIC", "CDATA_PIC_ORG"])` matches `7`, `95`,
    /// `CDATA_PIC` and `CDATA_PIC_ORG`
    Variant(GroupName, HashSet<String>),

    /// Matches if any of the given variants in the group are found somewhere in
    /// the node or its children.
    ///
    /// This is for matching things like `(8 * 10000 + 4)`, meaning
    /// `(ENCHANT_PROC * EXT_ENCHANT + ENCHANT_PROC_CHAOS_BALL.)`
    ///
    /// The `RecursiveCondition` is for specifying where in the expression
    /// should count as a match. For example, Lhs("*") would match the `8` to
    /// the left hand side of `8 * 10000`. This same logic also applies to the
    /// substitution rules.
    ///
    /// In the above expression, `VariantRecursive("enchant_id", ["ENCHANT_PROC"],
    /// Lhs("*"))` would match.
    VariantRecursive(GroupName, HashSet<String>, RecursiveCondition),

    /// Matches if the node looks like a flag, e.g. `(X + Y)`.
    ///
    /// Hackish workaround for expressions like `(250 + 4)`, which is supposed
    /// to be the constant `GDATA_FLAG_MAIN_SAGE`, where the terms are combined.
    Flag,

    /// Matches if the node is a variable, like `cc` or `cdata(0)`.
    Variable,

    /// Matches if the node is a variable or array with one of these names.
    Symbol(HashSet<String>),

    /// Matches if the index or function arguments in the node all match.
    ///
    /// `Array({0: Variant("cdata", ["CDATA_ID"])})` matches the expression
    /// `cdata(27, cc)`.
    Array(ArrayRules),

    /// Matches if the node is an expression like `(8 + 16)`, and not if it's a
    /// constant literal like `8`.
    Expr,

    /// Matches if the node is a constant literal like `8`, and not if it's an
    /// expression.
    Literal
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
enum RecursiveCondition {
    /// The constant can be anywhere in the expression.
    Any,

    /// The constant must be to the left hand side or in the unary position of
    /// an expression with the specified operator ("+", "-", etc.)
    Lhs(String),

    /// The constant must be to the right hand side of an expression with the
    /// specified operator ("+", "-", etc.)
    Rhs(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Substitute {
    /// Replace constant literals with the corresponding constant names found in
    /// this group.
    ///
    /// `Group("cdata")` replaces `27` with `CDATA_ID`.
    Group(GroupName),

    /// Replace constant literals with the corresponding constant names found in
    /// this group, recursively.
    ///
    /// `Group("cdata", Lhs("+"))` replaces `(100 + 100)` with
    /// `(CDATA_STARTING_EQUIP_SLOTS + 100)`.
    GroupRecursive(GroupName, RecursiveCondition),

    /// Replace constant literals with the specific variants from this group.
    Variant(GroupName, HashSet<String>),

    /// Replace constant literals with the specific variants from this group,
    /// recursively.
    ///
    /// `VariantRecursive("cdata", ["CDATA_STARTING_EQUIP_SLOTS"], Lhs("+"))`
    /// replaces `(100 + 100)` with `(CDATA_STARTING_EQUIP_SLOTS + 100)`, but
    /// leaves `(100 * 100)` unchanged.
    VariantRecursive(GroupName, HashSet<String>, RecursiveCondition),

    /// Replaces an integer constant with a call to `xy2pic(x, y)`.
    ///
    /// The formula is (i / 33, i % 33).
    ///
    /// `XY2Pic` replaces `67` with `xy2pic(2, 1)`.
    #[allow(non_snake_case)]
    XY2Pic,

    /// Runs each of the replacements in the specified order.
    Multi(Vec<Substitute>),

    /// Replaces flag expressions with the corresponding constant names from
    /// this group.
    ///
    /// Flag("gdata") replaces `(250 + 3)` with `(GDATA_FLAG_MAIN_FOOL)`.
    Flag(GroupName),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ArrayDefinition {
    indices: Vec<ArrayIndex>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ArrayIndex {
    #[serde(default)]
    rules: ArrayRules,
    substitute: ArraySubstitutes
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprDefinition {
    indices: Vec<ExprIndex>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum ExprIndex {
    MatchAll(ExprIndexAll),
    MatchAny(ExprIndexAny),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprIndexAll {
    #[serde(default)]
    rules: ExprRulesetAll,
    substitute: ExprSubstitutesAll,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprRulesetAll {
    #[serde(default)]
    lhs: Option<Rule>,

    #[serde(default)]
    ops: HashSet<String>,

    #[serde(default)]
    rhs: ArrayRules,
}

fn op_is_comparison(op: &PrimitiveToken) -> bool {
    match &op.dict_value.name[..] {
        "=" | "!" | ">" | "<" | ">=" | "<=" => true,
        _ => false
    }
}

impl ExprRulesetAll {
    fn op_matches(&self, op: &PrimitiveToken) -> bool {
        if self.ops.len() > 0 {
            self.ops.contains(&op.dict_value.name)
        } else {
            op_is_comparison(op)
        }
    }
}

impl Default for ExprRulesetAll {
    fn default() -> Self {
        ExprRulesetAll {
            lhs: None,
            ops: HashSet::new(),
            rhs: HashMap::new()
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprSubstitutesAll {
    rhs: ArraySubstitutes
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprIndexAny {
    #[serde(default)]
    rule: Option<Rule>,
    substitute: Substitute
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct FunctionDefinition {
    args: HashMap<usize, FunctionArg>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct FunctionArg {
    #[serde(default)]
    name: String,
    #[serde(default)]
    indices: Vec<FunctionArgIndex>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct FunctionArgIndex {
    #[serde(default)]
    rules: ArrayRules,
    substitute: Substitute
}

fn get_variable_name(node: &ast::AstNode, hsp3as: &Hsp3As) -> Option<String> {
    match &node.kind {
        ast::AstNodeKind::Variable(var) => {
            match &var.ident.kind {
                PrimitiveTokenKind::GlobalVariable(ref name) => Some(name.clone()),
                PrimitiveTokenKind::Parameter(ref param) => Some(hsp3as.param_names.get(&param).unwrap().to_string()),
                _ => None
            }
        },
        ast::AstNodeKind::Function(func) => Some(get_function_name(&func, &hsp3as)),
        _ => None
    }
}

fn get_function_name(node: &ast::FunctionNode, hsp3as: &Hsp3As) -> String {
    match node.ident.kind {
        PrimitiveTokenKind::UserFunction(func) |
        PrimitiveTokenKind::DllFunction(func) |
        PrimitiveTokenKind::ComFunction(func) => {
            hsp3as.function_names.get(&func).unwrap().to_string()
        },
        _ => node.ident.dict_value.name.clone()
    }
}

fn make_blank_token(kind: PrimitiveTokenKind) -> PrimitiveToken {
    PrimitiveToken {
        token_offset: 0,
        type_: 0,
        flag: PrimitiveTokenFlags::None,
        value: 0,
        // name: &'a str,
        dict_value: HspDictionaryValue::default(),
        kind: kind
    }
}

fn make_variable(name: String) -> ast::AstNodeKind {
    ast::AstNodeKind::Variable(ast::VariableNode {
        ident: make_blank_token(PrimitiveTokenKind::GlobalVariable(name)),
        arg: None
    })
}

fn generate_ambiguous_constant(names: &Vec<String>, i: i32) -> String {
    let mut buf = Vec::new();
    write!(&mut buf, "({} /*!!!@[", i).unwrap();
    for (i, name) in names.iter().enumerate() {
        write!(&mut buf, "{}", name).unwrap();
        if i < names.len() - 1 {
            write!(&mut buf, " @@@ ").unwrap();
        }
    }
    write!(&mut buf, "]@!!! */)").unwrap();
    std::str::from_utf8(buf.as_slice()).unwrap().to_string()
}

fn substitute_integer_constant(group: &VariableGroup, group_name: &str, exp: ast::AstNode, i: i32, allow: Option<&HashSet<String>>, diagnostics: &mut Diagnostics) -> ast::AstNode {
    let mut found = Vec::new();
    let mut primary = None;

    let mut compare = |var: &VariableDefinition| {
        if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
            if i == j as i32 {
                found.push(var.name.clone());
                if var.primary {
                    primary = Some(var.name.clone());
                    return true;
                }
            }
        }
        false
    };

    match allow {
        Some(allow) => {
            // Include variables with `exclude` set.
            for a in allow.iter() {
                let var = group._resolved_variables
                               .iter()
                               .find(|v| &v.name == a)
                               .ok_or_else(|| panic!("Could not find variable {} in group {}.", a, group_name)).unwrap();
                if compare(&var) {
                    break;
                }
            }
        },
        None => {
            for var in group.iter_resolved_variables() {
                if compare(&var) {
                    break;
                }
            }
        }
    }

    if found.len() > 0 {
        let name = if found.len() > 1 {
            if let Some(p) = primary {
                p
            } else {
                diagnostics.push(DiagnosticKind::Warning, format!("Ambiguous constant {} in group '{}'. Please correct manually.", i, group_name));
                generate_ambiguous_constant(&found, i)
            }
        } else {
            found.pop().unwrap()
        };
        let kind = make_variable(name);
        return ast::AstNode::new(exp.token_offset, kind, exp.tab_count);
    }

    if allow.is_some() || group.ignore.contains(&ron::Value::Number(ron::value::Number::Integer(i.into()))) {
        return exp;
    }

    diagnostics.push(DiagnosticKind::Error, format!("Could not find constant {} in group {}.", i, group_name));
    exp
}

fn expression_has_variant(exp: &ast::AstNode, group: &VariableGroup, variants: &HashSet<String>) -> bool {
    let mut any = false;

    let find = |var: &VariableDefinition| {
        match &exp.kind {
            ast::AstNodeKind::Literal(lit) => match &lit {
                ast::LiteralNode::Integer(i) => {
                    if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
                        *i == j as i32
                    } else {
                        unreachable!()
                    }
                },
                _ => false
            },
            ast::AstNodeKind::Variable(v) => match &v.ident.kind {
                PrimitiveTokenKind::GlobalVariable(s) => s == &var.name,
                _ => false,
            },
            _ => false
        }
    };

    if variants.len() > 0 {
        // Include variables with `exclude` set.
        for var in group._resolved_variables.iter().filter(|v| variants.contains(&v.name)) {
            any = true;
            if find(&var) {
                return true;
            }
        }
    } else {
        for var in group.iter_resolved_variables() {
            any = true;
            if find(&var) {
                return true;
            }
        }
    }

    if !any {
        panic!("Could not find any variants in {:?} in group", variants);
    }
    false
}

struct GroupRecursiveFindVisitor  {
    group: VariableGroup,
    group_name: String,
    variants: HashSet<String>,
    condition: RecursiveCondition,
    stack: Vec<RecursiveCondition>,
    found: bool
}

impl Visitor for GroupRecursiveFindVisitor {
    fn visit_node(&mut self, node: &ast::AstNode) {
        match &node.kind {
            ast::AstNodeKind::Literal(lit) => match lit {
                ast::LiteralNode::Integer(_) => {
                    let proceed = match &self.condition {
                        RecursiveCondition::Any => true,
                        x => self.stack.last().map_or(false, |c| x == c)
                    };
                    if proceed && expression_has_variant(node, &self.group, &self.variants) {
                        self.found = true
                    }
                },
                _ => {
                    self.stack.push(RecursiveCondition::Any);
                    visitor::visit_node(self, node);
                    self.stack.pop().unwrap();
                }
            },
            ast::AstNodeKind::Variable(_) => {
                let proceed = match &self.condition {
                    RecursiveCondition::Any => true,
                    x => self.stack.last().map_or(false, |c| x == c)
                };
                if proceed && expression_has_variant(node, &self.group, &self.variants) {
                    self.found = true
                } else {
                    self.stack.push(RecursiveCondition::Any);
                    visitor::visit_node(self, node);
                    self.stack.pop().unwrap();
                }
            },
            ast::AstNodeKind::Expression(ref exp) => {
                let lhs_cond = if let Some(op) = &exp.op {
                    RecursiveCondition::Lhs(op.to_string())
                } else {
                    RecursiveCondition::Any
                };
                {
                    self.stack.push(lhs_cond);
                    self.visit_node(&exp.lhs);
                    self.stack.pop().unwrap();
                }

                if let Some(ref rhs) = &exp.rhs {
                    let rhs_cond = if let Some(op) = &exp.op {
                        RecursiveCondition::Rhs(op.to_string())
                    } else {
                        RecursiveCondition::Any
                    };
                    {
                        self.stack.push(rhs_cond);
                        self.visit_node(&rhs);
                        self.stack.pop().unwrap();
                    }
                }
            }
            _ => {
                self.stack.push(RecursiveCondition::Any);
                visitor::visit_node(self, node);
                self.stack.pop().unwrap();
            }
        }
    }
}

struct GroupRecursiveVisitor<'a>  {
    group: VariableGroup,
    group_name: String,
    variants: Option<HashSet<String>>,
    condition: RecursiveCondition,
    stack: Vec<RecursiveCondition>,
    diagnostics: &'a mut Diagnostics
}

impl<'a> VisitorMut for GroupRecursiveVisitor<'a> {
    fn visit_node(&mut self, mut node: ast::AstNode) -> ast::AstNode {
        let n = node.clone();
        match &mut node.kind {
            ast::AstNodeKind::Literal(lit) => match lit {
                ast::LiteralNode::Integer(i) => {
                    let proceed = match &self.condition {
                        RecursiveCondition::Any => true,
                        x => self.stack.last().map_or(false, |c| x == c)
                    };
                    if proceed {
                        substitute_integer_constant(&self.group, &self.group_name, n, *i, self.variants.as_ref(), self.diagnostics)
                    } else {
                        node
                    }
                },
                _ => {
                    self.stack.push(RecursiveCondition::Any);
                    let node = visitor::visit_node_mut(self, node);
                    self.stack.pop().unwrap();
                    node
                }
            },
            ast::AstNodeKind::Expression(ref mut exp) => {
                let lhs_cond = if let Some(op) = &exp.op {
                    RecursiveCondition::Lhs(op.to_string())
                } else {
                    RecursiveCondition::Any
                };
                {
                    self.stack.push(lhs_cond);
                    exp.lhs = Box::new(self.visit_node(*exp.lhs.clone()));
                    self.stack.pop().unwrap();
                }

                if let Some(ref mut rhs) = &mut exp.rhs {
                    let rhs_cond = if let Some(op) = &exp.op {
                        RecursiveCondition::Rhs(op.to_string())
                    } else {
                        RecursiveCondition::Any
                    };
                    {
                        self.stack.push(rhs_cond);
                        *rhs = Box::new(self.visit_node(*rhs.clone()));
                        self.stack.pop().unwrap();
                    }
                }
                node
            }
            _ => {
                self.stack.push(RecursiveCondition::Any);
                let node = visitor::visit_node_mut(self, node);
                self.stack.pop().unwrap();
                node
            }
        }
    }
}

struct ConstantSubstitutionVisitor<'a> {
    config: &'a AnalysisConfig,
    hsp3as: &'a Hsp3As,
    visiting_function: Option<(FunctionDefinition, String)>,
    diagnostics: &'a mut Diagnostics
}

impl<'a> ConstantSubstitutionVisitor<'a> {
    fn constant_found_in_group(&self, group_name: &str, i: i32) -> bool {
        let group = self.config.variable_groups.get(group_name).unwrap();
        for var in group.iter_resolved_variables() {
            if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
                if i == j as i32 {
                    return true;
                }
            }
        }
        false
    }

    fn constant_name_found_in_group(&self, group_name: &str, name: &str) -> bool {
        let group = self.config.variable_groups.get(group_name).unwrap();
        for var in group.iter_resolved_variables() {
            if var.name == name {
                return true
            }
        }
        false
    }

    fn array_index_rule_matches(&self, exp: &ast::AstNode, rule: &Rule) -> bool {
        match rule {
            Rule::Any => true,
            Rule::Group(group_name) => {
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => self.constant_found_in_group(&group_name, *i),
                        _ => false
                    },
                    ast::AstNodeKind::Variable(var) => match &var.ident.kind {
                        PrimitiveTokenKind::GlobalVariable(s) => self.constant_name_found_in_group(&group_name, &s),
                        _ => false,
                    },
                    _ => false
                }
            },
            Rule::Constant(i) => {
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(j) => *i == *j,
                        _ => false
                    },
                    _ => false
                }
            },
            Rule::Variant(group_name, variants) => {
                let group = self.config.variable_groups.get(group_name).ok_or_else(|| panic!("Variable group {} not defined.", group_name)).unwrap();
                expression_has_variant(exp, &group, &variants)
            },
            Rule::VariantRecursive(group_name, variants, condition) => {
                let group = self.config.variable_groups.get(group_name).ok_or_else(|| panic!("Variable group {} not defined.", group_name)).unwrap();
                let mut visitor = GroupRecursiveFindVisitor {
                    group: group.clone(),
                    group_name: group_name.clone(),
                    variants: variants.clone(),
                    found: false,
                    condition: condition.clone(),
                    stack: Vec::new()
                };
                visitor.visit_node(&exp);
                visitor.found
            },
            Rule::Variable => match &exp.kind {
                ast::AstNodeKind::Variable(var) => var.arg.is_none(),
                _ => false
            },
            Rule::Symbol(syms) => match &exp.kind {
                ast::AstNodeKind::Variable(_) => {
                    let name = get_variable_name(&exp, &self.hsp3as).unwrap();
                    syms.contains(&name)
                },
                _ => false
            },
            Rule::Flag => match &exp.kind {
                ast::AstNodeKind::Expression(exp) => {
                    if let Some(rhs) = &exp.rhs {
                        if let Some(op) = &exp.op {
                            if op.dict_value.name == "+" {
                                if matches!(&exp.lhs.kind, ast::AstNodeKind::Literal(_))
                                    && matches!(&rhs.kind, ast::AstNodeKind::Literal(_))
                                {
                                    return true;
                                }
                            }
                        }
                    }
                    false
                },
                _ => false
            },
            Rule::Array(array_rules) => {
                match &exp.kind {
                    ast::AstNodeKind::Variable(var) => {
                        if let Some(node) = &var.arg {
                            if let ast::AstNodeKind::Argument(arg) = &node.kind {
                                if self.array_index_matches(arg, array_rules) {
                                    return true;
                                }
                            }
                        }
                        false
                    },
                    ast::AstNodeKind::Function(func) => {
                        if let Some(node) = &func.arg {
                            if let ast::AstNodeKind::Argument(arg) = &node.kind {
                                if self.array_index_matches(arg, array_rules) {
                                    return true;
                                }
                            }
                        }
                        false
                    },
                    _ => false
                }
            }
            Rule::Expr => !matches!(exp.kind, ast::AstNodeKind::Literal(_)),
            Rule::Literal => matches!(exp.kind, ast::AstNodeKind::Literal(_))
        }
    }

    fn array_index_matches(&self, arg: &ast::ArgumentNode, rules: &ArrayRules) -> bool {
        for (index, rule) in rules.iter() {
            if arg.exps.len() <= *index {
                return false;
            }

            if !self.array_index_rule_matches(&arg.exps[*index], &rule) {
                return false;
            }
        }

        true
    }

    fn substitute_xy2pic(&self, exp: &mut ast::AstNode, i: i32) {
        let code = format!("xy2pic({}, {})", i % 33, i / 33);
        let kind = ast::AstNodeKind::Literal(ast::LiteralNode::Symbol(code));
        *exp = ast::AstNode::new(exp.token_offset, kind, exp.tab_count);
    }

    fn substitute_array_index(&mut self, exp: &mut ast::AstNode, subst: &Substitute) {
        match subst {
            Substitute::Group(group_name) => {
                match self.config.variable_groups.get(group_name) {
                    Some(group) => match &exp.kind {
                        ast::AstNodeKind::Literal(lit) => match &lit {
                            ast::LiteralNode::Integer(i) => *exp = substitute_integer_constant(&group, &group_name, exp.clone(), *i, None, &mut self.diagnostics),
                            _ => ()
                        },
                        _ => ()
                    },
                    None => self.diagnostics.push(DiagnosticKind::Error, format!("Variable group {} not defined.", group_name))

                }
            },
            Substitute::GroupRecursive(group_name, condition) => {
                match self.config.variable_groups.get(group_name) {
                    Some(group) => {
                        let mut visitor = GroupRecursiveVisitor {
                            group: group.clone(),
                            group_name: group_name.clone(),
                            variants: None,
                            condition: condition.clone(),
                            stack: Vec::new(),
                            diagnostics: &mut self.diagnostics
                        };
                        *exp = visitor.visit_node(exp.clone());
                    },
                    None => self.diagnostics.push(DiagnosticKind::Error, format!("Variable group {} not defined.", group_name))
                }
            },
            Substitute::Variant(group_name, variants) => {
                match self.config.variable_groups.get(group_name) {
                    Some(group) => match &exp.kind {
                        ast::AstNodeKind::Literal(lit) => match &lit {
                            ast::LiteralNode::Integer(i) => *exp = substitute_integer_constant(&group, &group_name, exp.clone(), *i, Some(variants), &mut self.diagnostics),
                            _ => ()
                        },
                        _ => ()
                    },
                    None => self.diagnostics.push(DiagnosticKind::Error, format!("Variable group {} not defined.", group_name))

                }
            },
            Substitute::VariantRecursive(group_name, variants, condition) => {
                match self.config.variable_groups.get(group_name) {
                    Some(group) => {
                        let mut visitor = GroupRecursiveVisitor {
                            group: group.clone(),
                            group_name: group_name.clone(),
                            variants: Some(variants.clone()),
                            condition: condition.clone(),
                            stack: Vec::new(),
                            diagnostics: &mut self.diagnostics
                        };
                        *exp = visitor.visit_node(exp.clone());
                    },
                    None => self.diagnostics.push(DiagnosticKind::Error, format!("Variable group {} not defined.", group_name))
                }
            },
            Substitute::XY2Pic => {
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => self.substitute_xy2pic(exp, *i),
                        _ => ()
                    },
                    _ => ()
                }
            },
            Substitute::Flag(group_name) => {
                match self.config.variable_groups.get(group_name) {
                    Some(group) => {
                        let mut flag = None;
                        if let ast::AstNodeKind::Expression(exp) = &exp.kind {
                            let rhs = exp.rhs.as_ref().expect("Flag expression does not have a right hand side.");
                            if let ast::AstNodeKind::Literal(ast::LiteralNode::Integer(lhs_i)) = &exp.lhs.kind {
                                if let ast::AstNodeKind::Literal(ast::LiteralNode::Integer(rhs_i)) = rhs.kind {
                                    flag = Some(lhs_i + rhs_i);
                                }
                            }
                        }

                        match flag {
                            Some(flag) => {
                                *exp = substitute_integer_constant(&group, &group_name, exp.clone(), flag, None, &mut self.diagnostics);
                            },
                            None => {
                                self.diagnostics.push(DiagnosticKind::Error, format!("Expression does not look like a flag: {}", self.hsp3as.print_ast_node(exp).unwrap()));
                            }
                        }
                    },
                    None => self.diagnostics.push(DiagnosticKind::Error, format!("Variable group {} not defined.", group_name))
                }
            },
            Substitute::Multi(substs) => {
                for subst in substs.iter() {
                    self.substitute_array_index(exp, subst);
                }
            }
        }
    }

    fn substitute_array_indices(&mut self, arg: &mut ast::ArgumentNode, substitute: &ArraySubstitutes) {
        for (index, subst) in substitute.iter() {
            if arg.exps.len() <= *index {
                // panic!("Wanted to substitute index {}, but there are only {} arguments.", index, arg.exps.len());
                continue;
            }

            self.substitute_array_index(&mut arg.exps[*index], subst);
        }
    }

    fn expr_index_matches_all(&self, lhs: &ast::AstNode, rhs: &ast::AstNode, rules: &ExprRulesetAll, op: &PrimitiveToken) -> bool {
        if let Some(rule) = &rules.lhs {
            if !self.array_index_rule_matches(lhs, &rule) {
                return false;
            }
        }

        if !rules.op_matches(op) {
            return false;
        }

        match &rhs.kind {
            ast::AstNodeKind::Argument(arg) => {
                for (index, rule) in rules.rhs.iter() {
                    if arg.exps.len() <= *index {
                        return false;
                    }

                    if !self.array_index_rule_matches(&arg.exps[*index], &rule) {
                        return false;
                    }
                }

                true
            },
            _ => if let Some(rule) = rules.rhs.get(&0) {
                self.array_index_rule_matches(&rhs, &rule)
            } else {
                true
            }
        }
    }

    fn substitute_expr_indices_all(&mut self, rhs: &mut ast::AstNode, substitute: &ExprSubstitutesAll) {
        match &mut rhs.kind {
            ast::AstNodeKind::Argument(ref mut arg) => {
                self.substitute_array_indices(arg, &substitute.rhs);
            },
            _ => if let Some(rule) = substitute.rhs.get(&0) {
                self.substitute_array_index(rhs, rule);
            }
        }
    }

    fn expr_index_matches_any(&self, rhs: &ast::AstNode, rule: &Option<Rule>) -> Vec<usize> {
        let mut result = Vec::new();

        match &rhs.kind {
            ast::AstNodeKind::Argument(arg) => {
                for (index, exp) in arg.exps.iter().enumerate() {
                    if rule.as_ref().map_or(true, |r| self.array_index_rule_matches(&exp, &r)) {
                        result.push(index);
                    }
                }
            },
            _ => if rule.as_ref().map_or(true, |r| self.array_index_rule_matches(&rhs, &r)) {
                result.push(0);
            }
        }

        result
    }

    fn substitute_expr_indices_any(&mut self, rhs: &mut ast::AstNode, matching_indices: &Vec<usize>, substitute: &Substitute) {
        match &mut rhs.kind {
            ast::AstNodeKind::Argument(ref mut arg) => {
                for index in matching_indices.iter() {
                    let exp = arg.exps.get_mut(*index).unwrap();
                    self.substitute_array_index(exp, &substitute);
                }
            },
            _ => if matching_indices.len() > 0 {
                self.substitute_array_index(rhs, substitute)  
            } 
        }
    }

    fn match_expr_index(&mut self, a: &ast::AstNode, arg: &mut ast::AstNode, index: &ExprIndex, op: &PrimitiveToken) {
        match &index {
            ExprIndex::MatchAll(index) => {
                if self.expr_index_matches_all(a, &arg, &index.rules, op) {
                    self.substitute_expr_indices_all(arg, &index.substitute);
                }
            },
            ExprIndex::MatchAny(index) => {
                let matching_indices = self.expr_index_matches_any(&arg, &index.rule);
                self.substitute_expr_indices_any(arg, &matching_indices, &index.substitute);
            }
        }
    }

    // Try to propagate constants based on an expression including a function
    // parameter.
    fn find_func_indices(&mut self, var: &ast::AstNode) -> Option<Vec<FunctionArgIndex>> {
        if let ast::AstNodeKind::Variable(node) = &var.kind {
            if let PrimitiveTokenKind::Parameter(param) = &node.ident.kind {
                if let Some(param_name) = self.hsp3as.param_names.get(param) {
                    if let Some((func, funcname)) = &self.visiting_function {
                        if let Some(func_conf) = self.config.functions.get(funcname) {
                            for (_, arg) in func_conf.args.iter() {
                                let name = format!("{}_{}", funcname, arg.name);
                                if param_name == &name {
                                    return Some(arg.indices.clone());
                                }
                            }
                        } else {
                            self.diagnostics.push(DiagnosticKind::Error, format!("No function named '{}' declared", funcname));
                        }
                    }
                } else {
                    self.diagnostics.push(DiagnosticKind::Error, format!("No name for parameter {:?}", param));
                }
            }
        }

        None
    }
}

impl<'a> VisitorMut for ConstantSubstitutionVisitor<'a> {
    fn visit_variable(&mut self, mut node: ast::VariableNode) -> ast::VariableNode {
        if let Some(ref mut arg) = &mut node.arg {
            if let ast::AstNodeKind::Argument(ref mut arg) = arg.kind {
                let variable_name = match node.ident.kind {
                    PrimitiveTokenKind::GlobalVariable(ref name) => name.clone(),
                    PrimitiveTokenKind::Parameter(ref param) => self.hsp3as.param_names.get(&param).unwrap().to_string(),
                    _ => unreachable!()
                };
                if let Some(array_def) = &self.config.arrays.get(&variable_name) {
                    for index in array_def.indices.iter() {
                        if self.array_index_matches(arg, &index.rules) {
                            self.substitute_array_indices(arg, &index.substitute);
                        }
                    }
                }
            }
        }

        node
    }

    fn visit_assignment(&mut self, mut node: ast::AssignmentNode) -> ast::AssignmentNode {
        if let Some(ref mut arg) = &mut node.argument {
            if let Some(lhs_name) = get_variable_name(&node.var, &self.hsp3as) {
                if let Some(expr_def) = &self.config.expressions.get(&lhs_name) {
                    for index in expr_def.indices.iter() {
                        self.match_expr_index(&node.var, arg, index, &node.operator);
                    }
                }
            }
            if let Some(indices) = self.find_func_indices(&node.var) {
                for index in indices.iter() {
                    self.substitute_array_index(arg, &index.substitute);
                }
            }
        }

        node
    }

    fn visit_expression(&mut self, mut node: ast::ExpressionNode) -> ast::ExpressionNode {
        if let Some(ref op) = node.op {
            if let Some(ref mut rhs) = &mut node.rhs {
                if let Some(lhs_name) = get_variable_name(&node.lhs, &self.hsp3as) {
                    if let Some(expr_def) = &self.config.expressions.get(&lhs_name) {
                        for index in expr_def.indices.iter() {
                            self.match_expr_index(&node.lhs, rhs, index, op);
                        }
                    }
                }
                if op_is_comparison(op) {
                    if let Some(indices) = self.find_func_indices(&node.lhs) {
                        for index in indices.iter() {
                            self.substitute_array_index(rhs, &index.substitute);
                        }
                    }
                }

                if let Some(rhs_name) = get_variable_name(rhs, &self.hsp3as) {
                    if let Some(expr_def) = &self.config.expressions.get(&rhs_name) {
                        for index in expr_def.indices.iter() {
                            self.match_expr_index(&rhs, &mut node.lhs, index, op);
                        }
                    }
                }
                if op_is_comparison(op) {
                    if let Some(indices) = self.find_func_indices(&rhs) {
                        for index in indices.iter() {
                            self.substitute_array_index(&mut node.lhs, &index.substitute);
                        }
                    }
                }
            }
        }

        node
    }

    fn visit_function(&mut self, mut node: ast::FunctionNode) -> ast::FunctionNode {
        let name = get_function_name(&node, &self.hsp3as);
        if let Some(ref mut arg) = &mut node.arg {
            if let ast::AstNodeKind::Argument(ref mut arg) = arg.kind {

                if let Some(func_def) = &self.config.functions.get(&name) {
                    for (i, funcarg) in func_def.args.iter() {
                        let a = arg.clone();
                        if let Some(ref mut argnode) = arg.exps.get_mut(*i) {
                            for index in funcarg.indices.iter() {
                                if self.array_index_matches(&a, &index.rules) {
                                    self.substitute_array_index(argnode, &index.substitute);
                                }
                            }
                        }
                    }
                }
            }
        }

        node
    }

    fn visit_function_declaration(&mut self, funcdef: ast::FunctionDeclarationNode) -> ast::FunctionDeclarationNode {
        match funcdef.func.get_type() {
            Ax3FunctionType::DefFunc |
            Ax3FunctionType::DefCFunc => {
                match self.config.functions.get(&funcdef.default_name) {
                    Some(func_conf) => {
                        self.visiting_function = Some((func_conf.clone(), funcdef.default_name.clone()));
                        funcdef
                    },
                    None => {
                        self.diagnostics.push(DiagnosticKind::Error, format!("No function named '{}' declared", funcdef.default_name));
                        funcdef
                    }
                }
            },
            _ => funcdef
        }
    }
}

struct FunctionRenameVisitor<'a> {
    config: &'a AnalysisConfig,
    hsp3as: &'a mut Hsp3As,
    visiting_function: Option<(FunctionDefinition, String)>,
    seen_names: HashMap<String, String>,
    diagnostics: &'a mut Diagnostics
}

impl<'a> VisitorMut for FunctionRenameVisitor<'a> {
    fn visit_function_declaration(&mut self, funcdef: ast::FunctionDeclarationNode) -> ast::FunctionDeclarationNode {
        match funcdef.func.get_type() {
            Ax3FunctionType::DefFunc |
            Ax3FunctionType::DefCFunc => {
                match self.config.functions.get(&funcdef.default_name) {
                    Some(func_conf) => {
                        for (i, param) in funcdef.params.iter().enumerate() {
                            match func_conf.args.get(&i) {
                                Some(arg) => {
                                    self.hsp3as.param_names.entry(param.clone()).insert(format!("{}_{}", funcdef.default_name, arg.name));
                                },
                                None => self.diagnostics.push(DiagnosticKind::Error, format!("Missing argument {} for function '{}", i, funcdef.default_name)),
                            }
                        }
                        self.visiting_function = Some((func_conf.clone(), funcdef.default_name.clone()));
                        funcdef
                    },
                    None =>{
                        self.diagnostics.push(DiagnosticKind::Error, format!("No function named '{}' declared", funcdef.default_name));
                        funcdef
                    }
                }
            },
            _ => funcdef
        }
    }

    fn visit_variable(&mut self, mut var: ast::VariableNode) -> ast::VariableNode {
        match &mut var.ident.kind {
            PrimitiveTokenKind::GlobalVariable(ref mut name) => {
                if let Some(pos) = name.find("@") {
                    if !self.seen_names.contains_key(name) {
                        if let Some((_func_conf, default_name)) = &self.visiting_function {
                            let stripped = &name[..pos];
                            let new_name = format!("locvar_{}_{}", default_name, stripped);
                            self.seen_names.insert(name.clone(), new_name);
                        }
                    }
                    if let Some(new_name) = self.seen_names.get(name) {
                        *name = new_name.to_string();
                    }
                }
                var
            },
            _ => var
        }
    }
}

struct TxtUnrollVisitor<'a> {
    hsp3as: &'a Hsp3As,
    txt_function: Ax3Function
}

#[derive(Debug)]
enum TxtUnrollState {
    None,
    FoundTxtvalid(u32, u32),  // txtvalid = 0
    FoundTxtc1(u32, u32),     // txtc = 1 + ("" != "") + ("" != "") + ("" != "") + ("" != "") + ("" != "") + ("" != "") + ("" != "") + ("" != "")
    FoundTxtc2(u32, u32),     // txtc = rnd(txtc)
    FoundTxtSelect(u32, u32, ast::AstNodeRef), // txt_select lang("「Ｑ．九頭竜の城に住む深海の姫の名前は？」", "Which is correct name of turtle in Valm?"), "", "", "", "", "", "", "", ""
    FoundTcol(u32, u32, ast::AstNodeRef)       // tcol@txtfunc = 255, 255, 255
}

fn trim_arguments(args: &mut ast::ArgumentNode) {
    let mut index = None;
    for i in (0..args.exps.len()-1).rev() {
        let is_blank_string = match &args.exps[i].kind {
            ast::AstNodeKind::Literal(ast::LiteralNode::String(s)) => {
                s == ""
            },
            _ => false
        };

        if !is_blank_string {
            index = Some(i);
            break;
        }
    }

    let index = match index {
        Some(i) => i,
        None => 0
    };

    args.exps.drain(index+1..args.exps.len());
}

fn make_txt_node(token_offset: u32, tab_count: u32, args: ast::AstNode, txt_function: Ax3Function) -> ast::AstNode {
    let args_token_offset = args.token_offset;
    let args_tab_count = args.tab_count;
    let mut args = args.kind.into_argument().unwrap();

    trim_arguments(&mut args);
    let args_node = ast::AstNode::new(args_token_offset, ast::AstNodeKind::Argument(args), args_tab_count);

    // Macros aren't compiled into the bytecode, so cheat by creating a function
    // definition. The `#define global` will get added in by hand.
    let kind = ast::AstNodeKind::Function(ast::FunctionNode {
        ident: make_blank_token(PrimitiveTokenKind::UserFunction(txt_function)),
        arg: Some(Box::new(args_node))
    });
    ast::AstNode::new(token_offset, kind, tab_count)

}

impl<'a> VisitorMut for TxtUnrollVisitor<'a> {
    fn visit_block_statement_end(&mut self, mut block: ast::BlockStatementNode) -> ast::BlockStatementNode {
        let mut exps = Vec::new();
        let mut state = TxtUnrollState::None;

        for mut exp in block.nodes.into_iter() {
            let (exp, new_state) = match state {
                TxtUnrollState::FoundTxtc1(to, tc) => {
                    let assign = exp.kind.as_assignment().unwrap();
                    assert!(get_variable_name(&assign.var, &self.hsp3as).unwrap() == "txtc");
                    (None, TxtUnrollState::FoundTxtc2(to, tc))
                },
                TxtUnrollState::FoundTxtc2(to, tc) => {
                    let func = exp.kind.into_function().unwrap();
                    assert!(get_function_name(&func, self.hsp3as) == "txt_select");
                    (None, TxtUnrollState::FoundTxtSelect(to, tc, func.arg.unwrap()))
                },
                TxtUnrollState::FoundTxtSelect(to, tc, args) => {
                    let assign = exp.kind.into_assignment().unwrap();
                    assert!(get_variable_name(&assign.var, self.hsp3as).unwrap() == "tcol@txtfunc");
                    (None, TxtUnrollState::FoundTcol(to, tc, args))
                },
                TxtUnrollState::FoundTcol(to, tc, args) => {
                    exps.push(Box::new(make_txt_node(to, tc, *args, self.txt_function.clone())));
                    (Some(exp), TxtUnrollState::None)
                },
                state => {
                    if let ast::AstNodeKind::Assignment(assign) = &exp.kind {
                        if let ast::AstNodeKind::Variable(var) = &assign.var.kind {
                            if let PrimitiveTokenKind::GlobalVariable(ref name) = var.ident.kind {
                                if name == "txtvalid" {
                                    let to = exp.token_offset;
                                    let tc = exp.tab_count;
                                    (Some(exp), TxtUnrollState::FoundTxtvalid(to, tc))
                                } else {
                                    if name == "txtc" {
                                        match state {
                                            TxtUnrollState::None => {
                                                (None, TxtUnrollState::FoundTxtc1(exp.token_offset, exp.tab_count))
                                            },
                                            TxtUnrollState::FoundTxtvalid(to, tc) => {
                                                exps.pop().unwrap(); // discard previous `txtvalid = 0`
                                                (None, TxtUnrollState::FoundTxtc1(to, tc))
                                            },
                                            _ => (Some(exp), TxtUnrollState::None)
                                        }
                                    } else {
                                        (Some(exp), TxtUnrollState::None)
                                    }
                                }
                            } else {
                                (Some(exp), TxtUnrollState::None)
                            }
                        } else {
                            (Some(exp), TxtUnrollState::None)
                        }
                    } else {
                        (Some(exp), TxtUnrollState::None)
                    }
                }
            };
            state = new_state;

            if let Some(exp) = exp {
                exps.push(exp);
            }
        }

        match state {
            TxtUnrollState::None |
            TxtUnrollState::FoundTxtvalid(_, _) => (),
            TxtUnrollState::FoundTcol(to, tc, args) => {
                exps.push(Box::new(make_txt_node(to, tc, *args, self.txt_function.clone())));
            },
            state => panic!("Block ended in middle of txt call: {:?}", state)
        }

        block.nodes = exps;
        block
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
enum SourceMatchKind {
    If,
    /// NOTE: also matches +=, ++ and similar.
    Assignment,
    StringLiteral,
    Variable,
    Expression,
    Argument,
    Function,
    On,
    OnEvent
}

fn ast_node_matches(node: &ast::AstNode, kind: &SourceMatchKind) -> bool {
    use SourceMatchKind::*;

    match &node.kind {
        ast::AstNodeKind::IfStatement(_) => *kind == If,
        ast::AstNodeKind::Assignment(_) => *kind == Assignment,
        ast::AstNodeKind::Literal(ast::LiteralNode::String(_)) => *kind == StringLiteral,
        ast::AstNodeKind::Variable(_) => *kind == Variable,
        ast::AstNodeKind::Expression(_) => *kind == Expression,
        ast::AstNodeKind::Argument(_) => *kind == Argument,
        ast::AstNodeKind::Function(_) => *kind == Function,
        ast::AstNodeKind::OnStatement(_) => *kind == On,
        ast::AstNodeKind::OnEventStatement(_) => *kind == OnEvent,
        _ => false
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct LabelRenameRule {
    kind: SourceMatchKind,
    #[serde(rename = "match")]
    match_: String,
    #[serde(default)]
    exact: bool
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct LabelDefinition {
    #[serde(default)]
    rules: Vec<LabelRenameRule>,

    #[serde(default)]
    after: Option<String>,

    #[serde(skip_serializing, default)]
    _matched_so_far: usize
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum FileStartPoint {
    Label(String),
    Function(String),
}

impl FileStartPoint {
    fn matches(&self, name: &str) -> bool {
        match self {
            FileStartPoint::Label(s) |
            FileStartPoint::Function(s) => name == s
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct FileDefinition {
    begin: FileStartPoint,
    #[serde(default)]
    end: Option<FileStartPoint>,
}

struct LabelRenameVisitor<'a> {
    hsp3as: &'a mut Hsp3As,
    labels_remaining: HashMap<String, LabelDefinition>,
    resolved_labels: &'a mut HashMap<ResolvedLabel, LabelDefinition>,
    visiting_label: Option<ResolvedLabel>,
    previous_label: Option<ResolvedLabel>,
    diagnostics: &'a mut Diagnostics
}

impl<'a> LabelRenameVisitor<'a> {
    fn check_after_labels(&mut self) {
        // Only check if none of the rules matched so far.
        if self.visiting_label.is_none() {
            return;
        }

        let mut found_after = None;

        for (i, (label_name, defn)) in self.labels_remaining.iter_mut().enumerate() {
            defn._matched_so_far = 0;

            if defn.rules.len() == 0 {
                if let Some(prev) = &self.previous_label {
                    if let Some(after) = &defn.after {
                        if let Some(prev_resolved) = self.resolved_labels.get(prev) {
                            if self.hsp3as.label_names.get(prev).unwrap() == after {
                                assert!(found_after.is_none(), "More than one label with same 'after' field found: {}", after);
                                found_after = Some(label_name.clone());
                            }
                        }
                    }
                }
            }
        }

        if let Some(found) = found_after {
            let rule = self.labels_remaining.remove(&found).unwrap();
            let label = self.visiting_label.unwrap();
            *self.hsp3as.label_usage.entry(label.clone()).or_insert(0) += 1;
            self.associate_label(rule, found, &label);
        }
    }

    fn set_label(&mut self, label: ResolvedLabel) {
        // Exclude ghost labels
        if !self.hsp3as.label_usage.contains_key(&label) {
            return;
        }

        self.check_after_labels();

        if self.visiting_label.is_some() {
            self.previous_label = self.visiting_label.clone();
        } 
        self.visiting_label = Some(label);
    }

    fn associate_label(&mut self, rule: LabelDefinition, label_name: String, label: &ResolvedLabel) {
        self.previous_label = Some(label.clone());
        self.visiting_label = None;
        self.hsp3as.label_names.insert(label.clone(), label_name);

        match self.resolved_labels.get(label) {
            Some(existing) => {
                self.diagnostics.push(DiagnosticKind::Error,
                                      format!("Label {} was already resolved to rule {:?}. The regex must uniquely match across the entire program source.",
                                              self.hsp3as.label_names.get(&label).unwrap(), existing))
            },
            None => {
                self.resolved_labels.insert(label.clone(), rule);
            }
        }
    }
}

impl<'a> Visitor for LabelRenameVisitor<'a> {
    fn visit_node(&mut self, node: &ast::AstNode) {
        if self.visiting_label.is_some() {
            let mut matched = false;
            let mut found = None;
            let mut source = None;
            for (i, (label_name, defn)) in self.labels_remaining.iter_mut().enumerate() {
                if defn.rules.len() > 0 {
                    let rule = defn.rules.get_mut(defn._matched_so_far).unwrap();
                    if ast_node_matches(&node, &rule.kind) {
                        matched = true;
                        if source.is_none() {
                            source = Some(self.hsp3as.print_ast_node(node).unwrap());
                        }
                        let get = source.as_ref().map_or(false, |s| {
                            if rule.exact {
                                s == &rule.match_
                            } else {
                                s.contains(&rule.match_)
                            }
                        });
                        if get {
                            defn._matched_so_far += 1;
                            if defn._matched_so_far == defn.rules.len() {
                                found = Some(label_name.clone());
                                break;
                            }
                        }
                    }
                }
            }

            if matched {
                if let Some(label_name) = found {
                    let rule = self.labels_remaining.remove(&label_name).unwrap();
                    let label = self.visiting_label.unwrap();
                    self.associate_label(rule, label_name, &label);
                } else {
                    visitor::visit_node(self, node);
                }
            } else {
                visitor::visit_node(self, node);
            }
        } else {
            visitor::visit_node(self, node);
        }
    }

    fn visit_label_declaration(&mut self, node: &ast::LabelDeclarationNode) {
        self.set_label(node.label.clone());
    }

    fn visit_function_declaration(&mut self, node: &ast::FunctionDeclarationNode) {
        self.check_after_labels();
        self.visiting_label = None;
    }

    fn visit_program_end(&mut self, node: &ast::ProgramNode) {
        self.check_after_labels();
    }
}

struct LabelMergeVisitor<'a> {
    hsp3as: &'a mut Hsp3As,
    resolved_labels: &'a HashMap<ResolvedLabel, LabelDefinition>
}

impl<'a> VisitorMut for LabelMergeVisitor<'a> {
    fn visit_block_statement(&mut self, mut node: ast::BlockStatementNode) -> ast::BlockStatementNode {
        let mut merging = None;

        let mut result = Vec::new();

        for exp in node.nodes.into_iter() {
            let exp = match &exp.kind {
                ast::AstNodeKind::LabelDeclaration(target_label) => {
                    if self.hsp3as.label_usage.contains_key(&target_label.label) {
                        if let Some(merging_label) = merging {
                            let above_is_resolved = self.resolved_labels.contains_key(&merging_label);
                            let below_is_resolved = self.resolved_labels.contains_key(&target_label.label);

                            if above_is_resolved && below_is_resolved {
                                let kind = ast::AstNodeKind::LabelDeclaration(ast::LabelDeclarationNode {
                                    label: merging_label
                                });
                                let node = ast::AstNode::new(0, kind, 0);
                                result.push(Box::new(node));

                                merging = Some(target_label.label.clone());
                            } else {
                                let (resolved, to_remove) = if below_is_resolved {
                                    (target_label.label.clone(), merging_label.clone())
                                } else {
                                    (merging_label.clone(), target_label.label.clone())
                                };

                                let name = self.hsp3as.label_names.get(&resolved).unwrap().to_string();
                                self.hsp3as.label_names.entry(to_remove.clone()).insert(name);

                                self.hsp3as.label_usage.entry(to_remove.clone()).insert(0);
                                *self.hsp3as.label_usage.entry(resolved.clone()).or_insert(0) += 1;

                                merging = Some(resolved);
                            }

                            None
                        } else {
                            merging = Some(target_label.label.clone());
                            None
                        }
                    } else {
                        None
                    }
                },
                _ => {
                    if let Some(merging_label) = merging {
                        let kind = ast::AstNodeKind::LabelDeclaration(ast::LabelDeclarationNode {
                            label: merging_label
                        });
                        let node = ast::AstNode::new(0, kind, 0);
                        result.push(Box::new(node));
                    }
                    merging = None;
                    Some(exp)
                }
            };
            if let Some(exp) = exp {
                result.push(exp);
            }
        }

        node.nodes = result;
        node
    }
}

struct FileSplitVisitor<'a> {
    hsp3as: &'a Hsp3As,
    files: &'a HashMap<String, FileDefinition>,
    resolved_files: HashMap<String, ast::AstNode>,
    visiting_file: String,
    diagnostics: &'a mut Diagnostics
}

impl<'a> FileSplitVisitor<'a> {
    fn push_exp(&mut self, exp: ast::AstNodeRef) {
        let mut noderef = self.resolved_files.get_mut(&self.visiting_file).unwrap();
        noderef.kind.as_block_statement_mut().unwrap().nodes.push(exp);
    }

    fn push_blank_line(&mut self) {
        if self.exps_this_file() > 0 {
            self.push_exp(Box::new(ast::AstNode::new(0, ast::AstNodeKind::CommentLine(ast::CommentLineNode{ content: String::new() }), 0)));
        }
    }

    fn exps_this_file(&self) -> usize {
        match self.resolved_files.get(&self.visiting_file) {
            Some(noderef) => noderef.kind.as_block_statement().unwrap().nodes.len(),
            None => 0
        }
    }
}

impl<'a> Visitor for FileSplitVisitor<'a> {
    fn visit_node(&mut self, node: &ast::AstNode) {
        visitor::visit_node(self, node);
    }

    fn visit_program(&mut self, node: &ast::ProgramNode) {
        let block = node.block.kind.as_block_statement().unwrap();
        for node in block.nodes.iter() {
            node.visit(self);

            if !self.resolved_files.contains_key(&self.visiting_file) {
                let kind = ast::AstNodeKind::BlockStatement(ast::BlockStatementNode {
                    braces: false,
                    nodes: Vec::new(),
                });
                let node = ast::AstNode::new(0, kind, std::cmp::min(node.tab_count - 1, 0));
                self.resolved_files.insert(self.visiting_file.clone(), node);
            }

            self.push_exp(node.clone());
        }
    }

    fn visit_label_declaration(&mut self, node: &ast::LabelDeclarationNode) {
        if self.hsp3as.label_usage.contains_key(&node.label) {
            self.push_blank_line();

            for (filename, file) in self.files.iter() {
                let label_name = self.hsp3as.label_names.get(&node.label).unwrap();
                if file.begin.matches(&label_name) {
                    self.visiting_file = filename.clone();
                    break;
                }
            }
        }
    }

    fn visit_function_declaration(&mut self, node: &ast::FunctionDeclarationNode) {
        match node.func.get_type() {
            Ax3FunctionType::DefFunc |
            Ax3FunctionType::DefCFunc => {
                self.push_blank_line();
            },
            _ => ()
        }

        for (filename, file) in self.files.iter() {
            let function_name = node.get_name(self.hsp3as);
            if file.begin.matches(&function_name) {
                self.visiting_file = filename.clone();
                break;
            }
        }
    }

    fn visit_usedll_declaration(&mut self, node: &ast::UsedllDeclarationNode) {
        self.push_blank_line();
    }
}

fn merge_configs(config: &mut AnalysisConfig, parent: AnalysisConfig) -> Result<()> {
    // Variables
    for (group_name, group) in parent.variable_groups.into_iter() {
        if config.variable_groups.contains_key(&group_name) {
            let mut this_group = config.variable_groups.get_mut(&group_name).unwrap();
            for parent_variable in group.variables.into_iter() {
                let this_idx = this_group.variables.iter().position(|v| v.name == parent_variable.name);
                match this_idx {
                    Some(i) => this_group.variables.get_mut(i).unwrap().value = parent_variable.value.clone(),
                    None => this_group.variables.push(parent_variable.clone())
                }
            }
            for parent_include in group.includes.into_iter() {
                this_group.includes.insert(parent_include);
            }
            this_group.ignore = group.ignore;
        } else {
            config.variable_groups.insert(group_name, group);
        }
    }

    // Arrays
    for (array_name, array) in parent.arrays.into_iter() {
        config.arrays.insert(array_name, array);
    }

    // Expressions
    for (expr_name, expr) in parent.expressions.into_iter() {
        config.expressions.insert(expr_name, expr);
    }

    // Functions
    for (func_name, func) in parent.functions.into_iter() {
        config.functions.insert(func_name, func);
    }

    // Labels
    for (label_name, label) in parent.labels.into_iter() {
        config.labels.insert(label_name, label);
    }

    // Files
    for (file_name, file) in parent.files.into_iter() {
        config.files.insert(file_name, file);
    }

    Ok(())
}

fn merge_includes(config: &mut AnalysisConfig) -> Result<()> {
    for filename in config.includes.clone().iter() {
        let parent = load_config(filename)?;
        merge_configs(config, parent)?;
    }

    Ok(())
}

fn load_config(filename: &str) -> Result<AnalysisConfig> {
    let file = File::open(filename)?;
    let mut config: AnalysisConfig = ron::de::from_reader(file)?;
    merge_includes(&mut config)?;
    Ok(config)
}

fn resolve_variables(config: &AnalysisConfig, group: &VariableGroup) -> Vec<VariableDefinition> {
    let mut result = Vec::new();

    let mut vars = group.variables.clone();
    result.append(&mut vars);

    for include in group.includes.iter() {
        let mut vars = resolve_variables(config, config.variable_groups.get(include).ok_or_else(|| panic!("Cannot find variable group {}", include)).unwrap());
        result.append(&mut vars);
    }
    result
}

fn validate_config(config: &AnalysisConfig) -> Result<()> {
    for (group_name, group) in config.variable_groups.iter() {
        let mut seen_this_group = HashSet::new();
        for var in group.variables.iter() {
            if seen_this_group.contains(&var.name) {
                return Err(anyhow!("Duplicate variable definition in variable group '{}': {}", group_name, var.name));
            }
            seen_this_group.insert(var.name.clone());
        }
    }

    for (label_name, defn) in config.labels.iter() {
        if defn.rules.len() == 0 {
            match &defn.after {
                Some(after) => {
                    if !config.labels.iter().any(|(label_name, _)| label_name == after)  {
                        return Err(anyhow!("Label given in after property '{}' not declared (on label definition {})", after, label_name))
                    }
                },
                None => return Err(anyhow!("Label definition '{}' must have either 'rules' or 'after' field", label_name))
            }
            if defn.after.is_none() {
            }
        } else {
            if defn.after.is_some() {
                return Err(anyhow!("Label definition '{}' cannot have both 'rules' and 'after' fields", label_name))
            }
        }
    }

    Ok(())
}

pub fn detect_db_file(start_ax_bytes: &[u8]) -> Result<String> {
    let mut hasher = Sha256::new();
    hasher.update(start_ax_bytes);
    let sha256sum = format!("{:x}", hasher.finalize());

    for entry in fs::read_dir("./database")? {
        let path = entry?.path();
        if path.is_file() && path.extension().map_or(false, |e| e == "ron") {
            let mut file = File::open(&path)?;
            let config: AnalysisConfig = ron::de::from_reader(file)?;
            if &config.meta.ax_sha256 == &sha256sum {
                return Ok(path.into_os_string().into_string().unwrap());
            }
        }
    }

    Err(anyhow!("Sha256sum not found in database: {}", sha256sum))
}

pub fn analyze<'a>(hsp3as: &'a mut Hsp3As, opts: &AnalysisOptions) -> Result<AnalysisResult> {
    let mut config: AnalysisConfig = load_config(&opts.db_name)?;
    validate_config(&config)?;

    let mut resolved_vars = HashMap::new();
    for (name, group) in config.variable_groups.iter() {
        let vars = resolve_variables(&config, &group);
        resolved_vars.insert(name.clone(), vars);
    }

    for (name, group) in config.variable_groups.iter_mut() {
        let vars = resolved_vars.remove(name).unwrap();
        group._resolved_variables = vars;
    }

    let mut diagnostics = Diagnostics::new();
    let mut labels = config.labels.clone();

    let node = hsp3as.program.clone();

    println!("Unrolling txt...");

    let node = {
        let txt = hsp3as.add_function("txt".into(), Ax3FunctionType::DefFunc);

        let mut visitor = TxtUnrollVisitor {
            hsp3as: &hsp3as,
            txt_function: txt
        };

        visitor.visit_node(node)
    };

    println!("Renaming functions...");

    let node = {
        let mut visitor = FunctionRenameVisitor {
            config: &config,
            hsp3as: hsp3as,
            visiting_function: None,
            seen_names: HashMap::new(),
            diagnostics: &mut diagnostics
        };

        visitor.visit_node(node)
    };

    println!("Substituting constants...");

    let node = {
        let mut visitor = ConstantSubstitutionVisitor {
            config: &config,
            hsp3as: &hsp3as,
            visiting_function: None,
            diagnostics: &mut diagnostics
        };

        let node = visitor.visit_node(node);

        node
    };

    println!("Renaming labels...");

    let mut resolved_labels = HashMap::new();

    {
        let mut visitor = LabelRenameVisitor {
            hsp3as: hsp3as,
            labels_remaining: labels,
            resolved_labels: &mut resolved_labels,
            visiting_label: None,
            previous_label: None,
            diagnostics: &mut diagnostics
        };

        visitor.visit_node(&node);

        if visitor.labels_remaining.len() > 0 {
            for (label_name, _) in visitor.labels_remaining.iter() {
                diagnostics.push(DiagnosticKind::Error, format!("Error: No match for label rule {}", label_name));
            }
        }
    };

    println!("Merging labels...");

    let node = {
        let mut visitor = LabelMergeVisitor {
            hsp3as: hsp3as,
            resolved_labels: &resolved_labels
        };

        visitor.visit_node(node)
    };

    for (label, _) in hsp3as.label_usage.iter() {
        if !resolved_labels.contains_key(&label) {
            diagnostics.push(DiagnosticKind::Warning, format!("Unresolved label: {}", hsp3as.label_names.get(&label).unwrap()))
        }
    }

    println!("Splitting file...");

    let files = {
        let mut file_split = FileSplitVisitor {
            hsp3as: hsp3as,
            files: &config.files,
            resolved_files: HashMap::new(),
            visiting_file: String::from("main.hsp"),
            diagnostics: &mut diagnostics
        };

        file_split.visit_node(&node);
        file_split.resolved_files
    };

    let mut errors = 0;
    for diag in diagnostics.iter() {
        if diag.kind == DiagnosticKind::Error {
            errors += 1;
            println!("{}", diag);
        }
    }

    let strict = false;

    if errors == 0 || !strict {
        Ok(AnalysisResult {
            node: node,
            files: files
        })
    } else {
        Err(anyhow!("{} errors occured.", errors))
    }
}
