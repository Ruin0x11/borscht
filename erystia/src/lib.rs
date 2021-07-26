extern crate serde;
extern crate serde_derive;
extern crate ron;
extern crate anyhow;
extern crate exe2ax;

use std::collections::HashMap;
use std::fs::File;
use anyhow::Result;
use serde_derive::{Serialize, Deserialize};
use exe2ax::as_::ax3::Hsp3As;
use exe2ax::as_::ax3::ast;
use exe2ax::as_::ax3::lexical;
use exe2ax::as_::ax3::visitor::*;

pub struct AnalysisResult {

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
    variant: String
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
    Group(String),
    Variant(GroupVariant)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum Substitute {
    Group(String),
    XY2Pic
}


struct AnalysisVisitor {
    config: AnalysisConfig,
    hsp3as: Hsp3As
}

impl AnalysisVisitor {
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
            Rule::Group(group_name) => {
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) => self.constant_found_in_group(&group_name, *i),
                        ast::LiteralNode::String(s) => self.constant_name_found_in_group(&group_name, &s),
                        _ => false
                    },
                    _ => false
                }
            },
            Rule::Variant(group_variant) => {
                let group = self.config.variable_groups.get(&group_variant.name).unwrap();
                let var = group._resolved_variables.iter().find(|v| v.name == group_variant.name).unwrap();
                match &exp.kind {
                    ast::AstNodeKind::Literal(lit) => match &lit {
                        ast::LiteralNode::Integer(i) =>
                            if let ron::Value::Number(ron::value::Number::Integer(j)) = var.value {
                                *i == j as i32
                            } else {
                                unreachable!()
                            }
                        ast::LiteralNode::String(s) => s == &var.name,
                        _ => false
                    },
                    _ => false
                }
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

    fn array_matches(&self, arg: &ast::ArgumentNode, array_def: &ArrayDefinition) -> bool {
        array_def.indices.iter().any(|i| self.array_index_matches(arg, i))
    }
}

impl VisitorMut for AnalysisVisitor {
    fn visit_variable(&mut self, mut node: ast::VariableNode) -> ast::VariableNode {
        if let Some(ref mut arg) = &mut node.arg {
            if let ast::AstNodeKind::Argument(ref mut arg) = arg.kind {
                let variable_name = match node.ident.kind {
                    lexical::PrimitiveTokenKind::GlobalVariable(ref name) => name.clone(),
                    lexical::PrimitiveTokenKind::Parameter(ref param) => self.hsp3as.param_names.get(&param).unwrap().to_string(),
                    _ => unreachable!()
                };
                if let Some(array_def) = &self.config.arrays.get(&variable_name) {
                    if self.array_matches(&arg, &array_def) {
                        println!("Found match! {:?}", array_def);
                    }
                }
            }
        }

        node
    }
}

fn resolve_variables(config: &AnalysisConfig, group: &VariableGroup) -> Vec<VariableDefinition> {
    let mut result = Vec::new();
    for include in group.includes.iter() {
        let mut vars = resolve_variables(config, config.variable_groups.get(include).expect("Cannot find variable group"));
        result.append(&mut vars);
    }
    result
}

pub fn analyze(hsp3as: Hsp3As) -> Result<AnalysisResult> {
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
    let mut visitor = AnalysisVisitor {
        config: config,
        hsp3as: hsp3as
    };

    let node = visitor.visit_node(node);

    Ok(AnalysisResult {

    })
}
