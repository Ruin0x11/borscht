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
use exe2ax::as_::ax3::Hsp3As;
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
    name: String,
    value: ron::Value,
    #[serde(default)]
    code_value: Option<String>,
    #[serde(default)]
    exclude: bool,

    #[serde(default)]
    primary: bool
}

type ArrayRules = HashMap<usize, Rule>;
type ArraySubstitutes = HashMap<usize, Substitute>;

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Rule {
    Any,
    InGroup(String),
    Variant(GroupName, HashSet<String>),
    VariantRecursive(GroupName, HashSet<String>),
    Array(ArrayRules),
    Expr
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum RecursiveCondition {
    Any,
    Rhs(String),
    Lhs(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Substitute {
    Group(GroupName),
    GroupRecursive(GroupName, RecursiveCondition),
    VariantRecursive(GroupName, HashSet<String>, RecursiveCondition),
    #[allow(non_snake_case)]
    XY2Pic,
    Multi(Vec<Substitute>)
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
    #[serde(default)]
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

fn substitute_integer_constant(group: &VariableGroup, group_name: &str, exp: ast::AstNode, i: i32, allow: Option<&HashSet<String>>) -> ast::AstNode {
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

    println!("Could not find constant {} in group {}.", i, group_name);
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
    found: bool
}

impl Visitor for GroupRecursiveFindVisitor {
    fn visit_node(&mut self, node: &ast::AstNode) {
        if expression_has_variant(node, &self.group, &self.variants) {
            self.found = true
        } else {
            visitor::visit_node(self, node);
        }
    }
}

struct GroupRecursiveVisitor  {
    group: VariableGroup,
    group_name: String,
    variants: Option<HashSet<String>>,
    condition: RecursiveCondition
}

impl VisitorMut for GroupRecursiveVisitor {
    fn visit_node(&mut self, mut node: ast::AstNode) -> ast::AstNode {
        let n = node.clone();
        match &mut node.kind {
            ast::AstNodeKind::Literal(lit) => match lit {
                ast::LiteralNode::Integer(i) => {
                    substitute_integer_constant(&self.group, &self.group_name, n, *i, self.variants.as_ref())
                },
                _ => visitor::visit_node_mut(self, node)
            },
            ast::AstNodeKind::Expression(ref mut exp) => {
                let proceed = if let ast::AstNodeKind::Literal(_) = &exp.lhs.kind {
                    match &self.condition {
                        RecursiveCondition::Any => true,
                        RecursiveCondition::Lhs(reqop) => {
                            if let Some(op) = &exp.op {
                                &op.dict_value.name == reqop
                            } else {
                                false
                            }
                        },
                        _ => false
                    }
                } else {
                    true
                };
                if proceed {
                    exp.lhs = Box::new(self.visit_node(*exp.lhs.clone()));
                } 

                if let Some(ref mut rhs) = &mut exp.rhs {
                    let proceed = if let ast::AstNodeKind::Literal(_) = &rhs.kind {
                        match &self.condition {
                            RecursiveCondition::Any => true,
                            RecursiveCondition::Rhs(reqop) => {
                                if let Some(op) = &exp.op {
                                    &op.dict_value.name == reqop
                                } else {
                                    false
                                }
                            },
                            _ => false
                        }
                    } else {
                        true
                    };
                    if proceed {
                        *rhs = Box::new(self.visit_node(*rhs.clone()));
                    }
                }
                node
            }
            _ => visitor::visit_node_mut(self, node)
        }
    }
}

struct ConstantSubstitutionVisitor<'a> {
    config: AnalysisConfig,
    hsp3as: &'a Hsp3As
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
            Rule::Variant(group_name, variants) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                expression_has_variant(exp, &group, &variants)
            },
            Rule::VariantRecursive(group_name, variants) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                let mut visitor = GroupRecursiveFindVisitor { group: group.clone(), group_name: group_name.clone(), variants: variants.clone(), found: false };
                visitor.visit_node(&exp);
                visitor.found
            },
            Rule::Array(array_rules) => {
                if let ast::AstNodeKind::Variable(var) = &exp.kind {
                    if let Some(node) = &var.arg {
                        if let ast::AstNodeKind::Argument(arg) = &node.kind {
                            if self.array_index_matches(arg, array_rules) {
                                return true;
                            }
                        }
                    }
                }
                false
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

    fn substitute_array_index(&self, exp: &mut ast::AstNode, subst: &Substitute) {
        match subst {
            Substitute::Group(group_name) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => *exp = substitute_integer_constant(&group, &group_name, exp.clone(), *i, None),
                        _ => ()
                    },
                    _ => ()
                }
            },
            Substitute::GroupRecursive(group_name, condition) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                let mut visitor = GroupRecursiveVisitor { group: group.clone(), group_name: group_name.clone(), variants: None, condition: condition.clone() };
                *exp = visitor.visit_node(exp.clone());
            },
            Substitute::VariantRecursive(group_name, variants, condition) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                let mut visitor = GroupRecursiveVisitor { group: group.clone(), group_name: group_name.clone(), variants: Some(variants.clone()), condition: condition.clone() };
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
            Substitute::Multi(substs) => {
                for subst in substs.iter() {
                    self.substitute_array_index(exp, subst);
                }
            }
        }
    }

    fn substitute_array_indices(&self, arg: &mut ast::ArgumentNode, substitute: &ArraySubstitutes) {
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

    fn substitute_expr_indices_all(&self, rhs: &mut ast::AstNode, substitute: &ExprSubstitutesAll) {
        match &mut rhs.kind {
            ast::AstNodeKind::Argument(ref mut arg) => {
                self.substitute_array_indices(arg, &substitute.rhs);
            },
            _ => self.substitute_array_index(rhs, substitute.rhs.get(&0).unwrap())
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

    fn substitute_expr_indices_any(&self, rhs: &mut ast::AstNode, matching_indices: &Vec<usize>, substitute: &Substitute) {
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

    fn match_expr_index(&self, a: &ast::AstNode, arg: &mut ast::AstNode, index: &ExprIndex) {
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

                    if let Some(rhs_name) = self.get_variable_name(rhs) {
                        if let Some(expr_def) = &self.config.expressions.get(&rhs_name) {
                            for index in expr_def.indices.iter() {
                                self.match_expr_index(&rhs, &mut node.lhs, index);
                            }
                        }
                    }
                }
            }
        }

        node
    }

    fn visit_function(&mut self, mut node: ast::FunctionNode) -> ast::FunctionNode {
        if let Some(ref mut arg) = &mut node.arg {
            if let ast::AstNodeKind::Argument(ref mut arg) = arg.kind {
                let name = match node.ident.kind {
                    PrimitiveTokenKind::UserFunction(func) |
                    PrimitiveTokenKind::DllFunction(func) |
                    PrimitiveTokenKind::ComFunction(func) => {
                        self.hsp3as.function_names.get(&func).unwrap().to_string()
                    },
                    _ => node.ident.dict_value.name.clone()
                };

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

pub fn analyze<'a>(hsp3as: &'a Hsp3As) -> Result<AnalysisResult> {
    let file = File::open("database/vanilla.ron")?;
    let mut config: AnalysisConfig = ron::de::from_reader(file)?;

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
    let mut visitor = ConstantSubstitutionVisitor {
        config: config,
        hsp3as: &hsp3as
    };

    let node = visitor.visit_node(node);

    Ok(AnalysisResult {
        node: node
    })
}
