#![feature(entry_insert)]

extern crate serde;
extern crate serde_derive;
extern crate ron;
extern crate anyhow;
extern crate exe2ax;

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;
use anyhow::Result;
use serde_derive::{Serialize, Deserialize};
use exe2ax::as_::ax3::*;
use exe2ax::as_::ax3::ast;
use exe2ax::as_::dictionary::HspDictionaryValue;
use exe2ax::as_::ax3::lexical::*;
use exe2ax::as_::ax3::visitor::{self, *};

pub struct AnalysisResult {
    pub node: ast::AstNode
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct AnalysisConfig {
    variable_groups: HashMap<GroupName, VariableGroup>,
    arrays: HashMap<String, ArrayDefinition>,
    expressions: HashMap<String, ExprDefinition>,
    functions: HashMap<String, FunctionDefinition>
}

type GroupName = String;

#[derive(Clone, Debug, Serialize, Deserialize)]
struct VariableGroup {
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    includes: Vec<GroupName>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    variables: Vec<VariableDefinition>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    _resolved_variables: Vec<VariableDefinition>,
    #[serde(skip_serializing_if = "HashSet::is_empty", default)]
    ignore: HashSet<ron::Value>,
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
    /// `InGroup("cdata")` matches `32` and the variable name
    /// `CDATA_ATTACK_STYLE`.
    InGroup(String),

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

    /// Matches if the index or function arguments in the node all match.
    ///
    /// `Array({0: Variant("cdata", ["CDATA_ID"])})` matches the expression
    /// `cdata(27, cc)`.
    Array(ArrayRules),

    /// Matches if the node is an expression like `(8 + 16)`, and not if it's a
    /// constant literal like `8`.
    Expr
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
    substitute: ExprSubstitutesAll
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprRulesetAll {
    #[serde(default)]
    lhs: Option<Rule>,

    #[serde(default)]
    rhs: ArrayRules,
}

impl Default for ExprRulesetAll {
    fn default() -> Self {
        ExprRulesetAll {
            lhs: None,
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

fn make_variable(name: String) -> ast::AstNodeKind {
    ast::AstNodeKind::Variable(ast::VariableNode {
        ident: PrimitiveToken {
            token_offset: 0,
            type_: 0,
            flag: PrimitiveTokenFlags::None,
            value: 0,
            // name: &'a str,
            dict_value: HspDictionaryValue::default(),
            kind: PrimitiveTokenKind::GlobalVariable(name)
        },
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

fn substitute_integer_constant(group: &VariableGroup, group_name: &str, exp: ast::AstNode, i: i32, allow: Option<&HashSet<String>>, errors: &mut Vec<String>) -> ast::AstNode {
    let mut found = Vec::new();
    let mut primary = None;
    for var in group.iter_resolved_variables() {
        if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
            if i == j as i32 {
                if allow.map_or(false, |a| !a.contains(&var.name)) {
                    continue;
                }
                found.push(var.name.clone());
                if var.primary {
                    primary = Some(var.name.clone());
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
                println!("Ambiguous constant {} in group '{}'. Please correct manually.", i, group_name);
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

    errors.push(format!("Could not find constant {} in group {}.", i, group_name));
    exp
}

fn expression_has_variant(exp: &ast::AstNode, group: &VariableGroup, variants: &HashSet<String>) -> bool {
    let mut any = false;
    for var in group.iter_resolved_variables().filter(|v| variants.contains(&v.name)) {
        any = true;
        match &exp.kind {
            ast::AstNodeKind::Literal(lit) => match &lit {
                ast::LiteralNode::Integer(i) => {
                    if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
                        if *i == j as i32 {
                            return true;
                        }
                    } else {
                        unreachable!()
                    }
                },
                _ => ()
            },
            ast::AstNodeKind::Variable(v) => match &v.ident.kind {
                PrimitiveTokenKind::GlobalVariable(s) => { if s == &var.name { return true; } },
                _ => (),
            },
            _ => ()
        }
    }
    if !any {
        panic!("Could not find any variants in {:?} in group", variants);
    }
    false
}

fn op_is_comparison(op: &PrimitiveToken) -> bool {
    match &op.dict_value.name[..] {
        "=" | "!" | ">" | "<" | ">=" | "<=" => true,
        _ => false
    }
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
    errors: &'a mut Vec<String>
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
                        substitute_integer_constant(&self.group, &self.group_name, n, *i, self.variants.as_ref(), self.errors)
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
    errors: Vec<String>
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
            Rule::InGroup(group_name) => {
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
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                expression_has_variant(exp, &group, &variants)
            },
            Rule::VariantRecursive(group_name, variants, condition) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
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
            Rule::Expr => !matches!(exp.kind, ast::AstNodeKind::Literal(_))
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
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => *exp = substitute_integer_constant(&group, &group_name, exp.clone(), *i, None, &mut self.errors),
                        _ => ()
                    },
                    _ => ()
                }
            },
            Substitute::GroupRecursive(group_name, condition) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                let mut visitor = GroupRecursiveVisitor {
                    group: group.clone(),
                    group_name: group_name.clone(),
                    variants: None,
                    condition: condition.clone(),
                    stack: Vec::new(),
                    errors: &mut self.errors
                };
                *exp = visitor.visit_node(exp.clone());
            },
            Substitute::VariantRecursive(group_name, variants, condition) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                let mut visitor = GroupRecursiveVisitor {
                    group: group.clone(),
                    group_name: group_name.clone(),
                    variants: Some(variants.clone()),
                    condition: condition.clone(),
                    stack: Vec::new(),
                    errors: &mut self.errors
                };
                *exp = visitor.visit_node(exp.clone());
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
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));

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
                        *exp = substitute_integer_constant(&group, &group_name, exp.clone(), flag, None, &mut self.errors);
                    },
                    None => {
                        panic!("Expression does not look like a flag: {}", self.hsp3as.print_ast_node(exp).unwrap());
                    }
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

    fn get_variable_name(&self, node: &ast::AstNode) -> Option<String> {
        match &node.kind {
            ast::AstNodeKind::Variable(var) => {
                match &var.ident.kind {
                    PrimitiveTokenKind::GlobalVariable(ref name) => Some(name.clone()),
                    PrimitiveTokenKind::Parameter(ref param) => Some(self.hsp3as.param_names.get(&param).unwrap().to_string()),
                    _ => None
                }
            },
            ast::AstNodeKind::Function(func) => Some(get_function_name(&func, &self.hsp3as)),
            _ => None
        }
    }

    fn expr_index_matches_all(&self, lhs: &ast::AstNode, rhs: &ast::AstNode, rules: &ExprRulesetAll) -> bool {
        if let Some(rule) = &rules.lhs {
            if !self.array_index_rule_matches(lhs, &rule) {
                return false;
            }
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

    fn match_expr_index(&mut self, a: &ast::AstNode, arg: &mut ast::AstNode, index: &ExprIndex) {
        match &index {
            ExprIndex::MatchAll(index) => {
                if self.expr_index_matches_all(a, &arg, &index.rules) {
                    self.substitute_expr_indices_all(arg, &index.substitute);
                }
            },
            ExprIndex::MatchAny(index) => {
                let matching_indices = self.expr_index_matches_any(&arg, &index.rule);
                self.substitute_expr_indices_any(arg, &matching_indices, &index.substitute);
            }
        }
    }

    fn find_func_indices(&self, var: &ast::AstNode) -> Option<Vec<FunctionArgIndex>> {
        if let ast::AstNodeKind::Variable(node) = &var.kind {
            if let PrimitiveTokenKind::Parameter(param) = &node.ident.kind {
                let param_name = self.hsp3as.param_names.get(param).expect(&format!("No name for parameter {:?}", param));
                if let Some((func, funcname)) = &self.visiting_function {
                    let func_conf = self.config.functions.get(funcname).expect(&format!("No function named '{}' declared", funcname));
                    for (_, arg) in func_conf.args.iter() {
                        let name = format!("{}_{}", funcname, arg.name);
                        if param_name == &name {
                            return Some(arg.indices.clone());
                        }
                    }
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
            if let Some(lhs_name) = self.get_variable_name(&node.var) {
                if let Some(expr_def) = &self.config.expressions.get(&lhs_name) {
                    for index in expr_def.indices.iter() {
                        self.match_expr_index(&node.var, arg, index);
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
            if op_is_comparison(op) {
                if let Some(ref mut rhs) = &mut node.rhs {
                    if let Some(lhs_name) = self.get_variable_name(&node.lhs) {
                        if let Some(expr_def) = &self.config.expressions.get(&lhs_name) {
                            for index in expr_def.indices.iter() {
                                self.match_expr_index(&node.lhs, rhs, index);
                            }
                        }
                    }
                    if let Some(indices) = self.find_func_indices(&node.lhs) {
                        for index in indices.iter() {
                            self.substitute_array_index(rhs, &index.substitute);
                        }
                    }

                    if let Some(rhs_name) = self.get_variable_name(rhs) {
                        if let Some(expr_def) = &self.config.expressions.get(&rhs_name) {
                            for index in expr_def.indices.iter() {
                                self.match_expr_index(&rhs, &mut node.lhs, index);
                            }
                        }
                    }
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
                let func_conf = self.config.functions.get(&funcdef.default_name).expect(&format!("No function named '{}' declared", funcdef.default_name));
                self.visiting_function = Some((func_conf.clone(), funcdef.default_name.clone()));
                funcdef
            },
            _ => funcdef
        }
    }
}

struct FunctionRenameVisitor<'a> {
    config: &'a AnalysisConfig,
    hsp3as: &'a mut Hsp3As,
    visiting_function: Option<(FunctionDefinition, String)>,
    seen_names: HashMap<String, String>
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
                                None => println!("Missing argument {} for function '{}", i, funcdef.default_name),
                            }
                        }
                        self.visiting_function = Some((func_conf.clone(), funcdef.default_name.clone()));
                        funcdef
                    },
                    None =>{
                        println!("No function named '{}' declared", funcdef.default_name);
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

struct TxtUnrollVisitor {

}

struct ChatListUnrollVisitor {

}

fn resolve_variables(config: &AnalysisConfig, group: &VariableGroup) -> Vec<VariableDefinition> {
    let mut result = Vec::new();

    let mut vars = group.variables.clone();
    result.append(&mut vars);

    for include in group.includes.iter() {
        let mut vars = resolve_variables(config, config.variable_groups.get(include).expect(&format!("Cannot find variable group {}", include)));
        result.append(&mut vars);
    }
    result
}

pub fn analyze<'a>(hsp3as: &'a mut Hsp3As) -> Result<AnalysisResult> {
    let file = File::open("database/vanilla.ron")?;
    let mut config: AnalysisConfig = ron::de::from_reader(file)?;

    let strict = true;

    let mut resolved_vars = HashMap::new();
    for (name, group) in config.variable_groups.iter() {
        let vars = resolve_variables(&config, &group);
        resolved_vars.insert(name.clone(), vars);
    }

    for (name, group) in config.variable_groups.iter_mut() {
        let vars = resolved_vars.remove(name).unwrap();
        group._resolved_variables = vars;
    }

    let node = hsp3as.program.clone();

    let node = {
        let mut visitor = FunctionRenameVisitor {
            config: &config,
            hsp3as: hsp3as,
            visiting_function: None,
            seen_names: HashMap::new()
        };

        visitor.visit_node(node)
    };

    let node = {
        let mut visitor = ConstantSubstitutionVisitor {
            config: &config,
            hsp3as: &hsp3as,
            visiting_function: None,
            errors: Vec::new()
        };

        let node = visitor.visit_node(node);

        if strict && visitor.errors.len() > 0 {
            for error in visitor.errors.iter() {
                println!("Error: {}", error);
            }
            panic!("Errors occurred.");
        }

        node
    };

    let node = {
        let mut visitor = TxtUnrollVisitor {};

        visitor.visit_node(node)
    };

    let node = {
        let mut visitor = ChatListUnrollVisitor {};

        visitor.visit_node(node)
    };

    Ok(AnalysisResult {
        node: node
    })
}
