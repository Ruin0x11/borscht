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
use exe2ax::as_::ax3::visitor::*;

pub struct AnalysisResult {
    pub node: ast::AstNode
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct AnalysisConfig {
    variable_groups: HashMap<GroupName, VariableGroup>,
    arrays: HashMap<String, ArrayDefinition>
}

type GroupName = String;

#[derive(Clone, Debug, Serialize, Deserialize)]
struct VariableGroup {
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    includes: Vec<GroupName>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    variables: Vec<VariableDefinition>,

    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    _resolved_variables: Vec<VariableDefinition>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct VariableDefinition {
    name: String,
    value: ron::Value
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct GroupVariant {
    name: GroupName,
    variants: HashSet<String>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ArrayDefinition {
    indices: Vec<ArrayIndex>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ArrayIndex {
    rules: HashMap<usize, Rule>,
    substitute: HashMap<usize, Substitute>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Rule {
    Any,
    InGroup(String),
    Variant(GroupVariant)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Substitute {
    Group(String),
    XY2Pic
}


struct ConstantSubstitutionVisitor<'a> {
    config: AnalysisConfig,
    hsp3as: &'a Hsp3As
}

impl<'a> ConstantSubstitutionVisitor<'a> {
    fn constant_found_in_group(&self, group_name: &str, i: i32) -> bool {
        let group = self.config.variable_groups.get(group_name).unwrap();
        for var in group._resolved_variables.iter() {
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
        for var in group._resolved_variables.iter() {
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
            Rule::Variant(group_variant) => {
                let group = self.config.variable_groups.get(&group_variant.name).expect("Variable group not defined.");
                for var in group._resolved_variables.iter().filter(|v| group_variant.variants.contains(&v.name)) {
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
            }
        }
    }

    fn array_index_matches(&self, arg: &ast::ArgumentNode, array_index: &ArrayIndex) -> bool {
        for (index, rule) in array_index.rules.iter() {
            if arg.exps.len() <= *index {
                return false;
            }

            if !self.array_index_rule_matches(&arg.exps[*index], &rule) {
                return false;
            }
        }

        true
    }

    fn make_variable(&self, name: String) -> ast::AstNodeKind {
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

    fn substitute_integer_constant(&self, group_name: &str, exp: &mut ast::AstNode, i: i32) {
        let group = self.config.variable_groups.get(group_name).expect("Variable group not defined.");
        for var in group._resolved_variables.iter() {
            if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
                if i == j as i32 {
                    let kind = self.make_variable(var.name.clone());
                    *exp = ast::AstNode::new(exp.token_offset, kind, exp.tab_count);
                    return;
                }
            }
        }
        panic!("Could not find constant {} in group {}.", i, group_name)
    }

    fn substitute_xy2pic(&self, exp: &mut ast::AstNode, i: i32) {
        let code = format!("xy2pic({}, {})", i % 33, i / 33);
        println!("SUBST xy2pic {}", code);
        let kind = ast::AstNodeKind::Literal(ast::LiteralNode::Symbol(code));
        *exp = ast::AstNode::new(exp.token_offset, kind, exp.tab_count);
    }

    fn substitute_array_index(&self, exp: &mut ast::AstNode, subst: &Substitute) {
        match subst {
            Substitute::Group(group_name) => {
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => self.substitute_integer_constant(&group_name, exp, *i),
                        _ => ()
                    },
                    _ => ()
                }
            },
            XY2Pic => {
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => self.substitute_xy2pic(exp, *i),
                        x => ()
                    },
                    x => ()
                }
            }
        }
    }

    fn substitute_array_indices(&self, arg: &mut ast::ArgumentNode, array_index: &ArrayIndex) {
        for (index, subst) in array_index.substitute.iter() {
            if arg.exps.len() <= *index {
                panic!("Wanted to substitute index {}, but there are only {} arguments.", index, arg.exps.len());
            }

            self.substitute_array_index(&mut arg.exps[*index], subst);
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
                        if self.array_index_matches(arg, index) {
                            println!("Found match! {:?}", index);
                            self.substitute_array_indices(arg, &index);
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
        let mut vars = resolve_variables(config, config.variable_groups.get(include).expect("Cannot find variable group"));
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
