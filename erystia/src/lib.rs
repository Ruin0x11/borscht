extern crate serde;
extern crate serde_derive;
extern crate ron;
extern crate anyhow;
extern crate exe2ax;

use std::collections::{HashMap, HashSet};
use std::fs::File;
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
    expressions: HashMap<String, ExprDefinition>
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
    exclude: bool
}

type ArrayRules = HashMap<usize, Rule>;
type ArraySubstitutes = HashMap<usize, Substitute>;

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Rule {
    Any,
    InGroup(String),
    Variant(GroupName, HashSet<String>),
    Array(ArrayRules)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Substitute {
    Group(String),
    GroupRecursive(String),
    #[allow(non_snake_case)]
    XY2Pic
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ArrayDefinition {
    indices: Vec<ArrayIndex>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ArrayIndex {
    rules: ArrayRules,
    substitute: ArraySubstitutes
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprDefinition {
    indices: Vec<ExprIndex>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprIndex {
    #[serde(default)]
    rules: ExprRuleset,
    substitute: ExprSubstitutes
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprRuleset {
    #[serde(default)]
    lhs: Option<Rule>,

    #[serde(default)]
    rhs: ArrayRules,
}

impl Default for ExprRuleset {
    fn default() -> Self {
        ExprRuleset {
            lhs: None,
            rhs: HashMap::new()
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ExprSubstitutes {
    #[serde(default)]
    rhs: ArraySubstitutes
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

fn substitute_integer_constant(group: &VariableGroup, group_name: &str, exp: ast::AstNode, i: i32) -> ast::AstNode {
    for var in group.iter_resolved_variables() {
        if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
            if i == j as i32 {
                let kind = make_variable(var.name.clone());
                return ast::AstNode::new(exp.token_offset, kind, exp.tab_count);
            }
        }
    }

    if group.ignore.contains(&ron::Value::Number(ron::value::Number::Integer(i.into()))) {
        return exp;
    }

    panic!("Could not find constant {} in group {}.", i, group_name)
}


struct GroupRecursiveVisitor  {
    group: VariableGroup,
    group_name: String
}

impl VisitorMut for GroupRecursiveVisitor {
    fn visit_node(&mut self, mut node: ast::AstNode) -> ast::AstNode {
        match &node.kind {
            ast::AstNodeKind::Literal(lit) => match lit {
                ast::LiteralNode::Integer(i) => {
                    substitute_integer_constant(&self.group, &self.group_name, node.clone(), *i)
                },
                _ => visitor::visit_node_mut(self, node)
            },
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
                for var in group.iter_resolved_variables().filter(|v| variants.contains(&v.name)) {
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
                            PrimitiveTokenKind::GlobalVariable(s) => if s == &var.name { return true; },
                            _ => (),
                        },
                        _ => ()
                    }
                }
                false
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
                        ast::LiteralNode::Integer(i) => *exp = substitute_integer_constant(&group, &group_name, exp.clone(), *i),
                        _ => ()
                    },
                    _ => ()
                }
            },
            Substitute::GroupRecursive(group_name) => {
                let group = self.config.variable_groups.get(group_name).expect(&format!("Variable group {} not defined.", group_name));
                let mut visitor = GroupRecursiveVisitor { group: group.clone(), group_name: group_name.clone() };
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
            }
        }
    }

    fn substitute_array_indices(&self, arg: &mut ast::ArgumentNode, substitute: &ArraySubstitutes) {
        for (index, subst) in substitute.iter() {
            if arg.exps.len() <= *index {
                panic!("Wanted to substitute index {}, but there are only {} arguments.", index, arg.exps.len());
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

    fn expr_index_matches(&self, lhs: &ast::AstNode, rhs: &ast::AstNode, rules: &ExprRuleset) -> bool {
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
            _ => unreachable!()
        }
    }

    fn substitute_expr_indices(&self, rhs: &mut ast::AstNode, substitute: &ExprSubstitutes) {
        if let ast::AstNodeKind::Argument(ref mut arg) = &mut rhs.kind {
            println!("Substitute! {:?}", arg);
            self.substitute_array_indices(arg, &substitute.rhs);
        } else {
            panic!("Cannot substitute expression RHS: {:?}", rhs)
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
                        if self.expr_index_matches(&node.var, &arg, &index.rules) {
                            println!("Found match! {:?}", index);
                            self.substitute_expr_indices(arg, &index.substitute);
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
