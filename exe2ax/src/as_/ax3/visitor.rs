use super::ast::*;
use paste;

macro_rules! create_visitor {
    ($($visit_name:ident => $ast_variant:ident($ast_type:ident),)+) => {
        pub fn visit_node<V: Visitor + Sized>(visitor: &mut V, node: &AstNode) {
            match &node.kind {
                $(
                    AstNodeKind::$ast_variant(node) => node.visit(visitor),
                )+
            }
        }

        pub trait Visitor {
            fn visit_node(&mut self, node: &AstNode) where Self: Sized {
                visit_node(self, node);
            }

            paste::item! {
                $(
                    #[allow(missing_docs)]
                    fn $visit_name(&mut self, _node: &$ast_type) { }
                    #[allow(missing_docs)]
                    fn [<$visit_name _end>](&mut self, _node: &$ast_type) { }
                )+
            }
        }

        pub fn visit_node_mut<V: VisitorMut + Sized>(visitor: &mut V, node: AstNode) -> AstNode {
            let token_offset = node.token_offset;
            let tab_count = node.tab_count;
            let kind = match node.kind {
                $(
                    AstNodeKind::$ast_variant(node) => AstNodeKind::$ast_variant(node.visit_mut(visitor)),
                )+
            };

            AstNode::new(token_offset, kind, tab_count)
        }

        /// A trait that implements functions to listen for specific nodes/tokens.
        /// Unlike [`Visitor`], nodes/tokens passed are mutable.
        pub trait VisitorMut {
            fn visit_node(&mut self, node: AstNode) -> AstNode where Self: Sized {
                visit_node_mut(self, node)
            }

            paste::item! {
                $(
                    #[allow(missing_docs)]
                    fn $visit_name(&mut self, node: $ast_type) -> $ast_type {
                        node
                    }

                    #[allow(missing_docs)]
                    fn [<$visit_name _end>](&mut self, node: $ast_type) -> $ast_type {
                        node
                    }
                )+
            }
        }
    };
}

#[doc(hidden)]
pub trait Visit {
    fn visit<V: Visitor>(&self, visitor: &mut V);
}

#[doc(hidden)]
pub trait VisitMut
where
    Self: Sized,
{
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self;
}

impl<T: Visit> Visit for &T {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<T: Visit> Visit for &mut T {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<T: Visit> Visit for Vec<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        for item in self {
            item.visit(visitor);
        }
    }
}

impl<T: VisitMut> VisitMut for Vec<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        self.into_iter()
            .map(|item| item.visit_mut(visitor))
            .collect()
    }
}

impl<T: Visit> Visit for Option<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        if let Some(item) = self {
            item.visit(visitor);
        }
    }
}

impl<T: VisitMut> VisitMut for Option<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        self.map(|item| item.visit_mut(visitor))
    }
}

impl<A: Visit, B: Visit> Visit for (A, B) {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        self.0.visit(visitor);
        self.1.visit(visitor);
    }
}

impl<A: VisitMut, B: VisitMut> VisitMut for (A, B) {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        (self.0.visit_mut(visitor), self.1.visit_mut(visitor))
    }
}

impl<T: Visit> Visit for Box<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<T: VisitMut> VisitMut for Box<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        Box::new((*self).visit_mut(visitor))
    }
}


impl Visit for AstNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_node(self);
    }
}

impl VisitMut for AstNode {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> AstNode {
        visitor.visit_node(self)
    }
}

create_visitor! {
    visit_program => Program(ProgramNode),
    visit_comment_line => CommentLine(CommentLineNode),
    visit_if_statement => IfStatement(IfStatementNode),
    visit_assignment => Assignment(AssignmentNode),
    visit_literal => Literal(LiteralNode),
    visit_variable => Variable(VariableNode),
    visit_expression => Expression(ExpressionNode),
    visit_argument => Argument(ArgumentNode),
    visit_function => Function(FunctionNode),
    visit_on_statement => OnStatement(OnStatementNode),
    visit_on_event_statement => OnEventStatement(OnEventStatementNode),
    visit_block_statement => BlockStatement(BlockStatementNode),
    visit_label_declaration => LabelDeclaration(LabelDeclarationNode),
    visit_function_declaration => FunctionDeclaration(FunctionDeclarationNode),
    visit_usedll_declaration => UsedllDeclaration(UsedllDeclarationNode),
    visit_command_statement => CommandStatement(CommandStatementNode),
    visit_mcall_statement => McallStatement(McallStatementNode),
}

impl Visit for ProgramNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_program(self);

        self.block.visit(visitor);
    }
}

impl VisitMut for ProgramNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_program(self);

        self.block = self.block.visit_mut(visitor);

        self
    }
}

impl Visit for CommentLineNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_comment_line(self);
    }
}

impl VisitMut for CommentLineNode {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        visitor.visit_comment_line(self)
    }
}

impl Visit for IfStatementNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_if_statement(self);

        if let Some(arg) = &self.arg {
            arg.visit(visitor);
        }
        self.if_block.visit(visitor);
        if let Some(else_part) = &self.else_part {
            else_part.block.visit(visitor);
        }
    }
}

impl VisitMut for IfStatementNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_if_statement(self);

        self.arg = self.arg.map(|a| a.visit_mut(visitor));
        self.if_block = self.if_block.visit_mut(visitor);
        self.else_part = self.else_part.map(|mut e| { e.block = e.block.visit_mut(visitor); e });

        self
    }
}

impl Visit for AssignmentNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_assignment(self);

        self.var.visit(visitor);
        if let Some(argument) = &self.argument {
            argument.visit(visitor);
        }
    }
}

impl VisitMut for AssignmentNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_assignment(self);

        self.var = self.var.visit_mut(visitor);
        self.argument = self.argument.map(|a| a.visit_mut(visitor));

        self
    }
}

impl Visit for LiteralNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_literal(self);
    }
}

impl VisitMut for LiteralNode {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        visitor.visit_literal(self)
    }
}

impl Visit for VariableNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_variable(self);

        if let Some(arg) = &self.arg {
            arg.visit(visitor);
        }
    }
}

impl VisitMut for VariableNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_variable(self);

        self.arg = self.arg.map(|a| a.visit_mut(visitor));

        self
    }
}

impl Visit for ExpressionNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_expression(self);

        self.lhs.visit(visitor);

        if let Some(rhs) = &self.rhs {
            rhs.visit(visitor);
        }
    }
}

impl VisitMut for ExpressionNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_expression(self);

        self.lhs = self.lhs.visit_mut(visitor);
        self.rhs = self.rhs.map(|r| r.visit_mut(visitor));

        self
    }
}

impl Visit for ArgumentNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_argument(self);

        for exp in self.exps.iter() {
            exp.visit(visitor);
        }
    }
}

impl VisitMut for ArgumentNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_argument(self);

        self.exps = self.exps.into_iter().map(|e| e.visit_mut(visitor)).collect::<_>();

        self
    }
}

impl Visit for FunctionNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_function(self);

        if let Some(arg) = &self.arg {
            arg.visit(visitor);
        }
    }
}

impl VisitMut for FunctionNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_function(self);

        self.arg = self.arg.map(|a| a.visit_mut(visitor));

        self
    }
}

impl Visit for OnStatementNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_on_statement(self);

        if let Some(exp) = &self.exp {
            exp.visit(visitor);
        }

        if let Some(func) = &self.func {
            func.visit(visitor);
        }
    }
}

impl VisitMut for OnStatementNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_on_statement(self);

        self.exp = self.exp.map(|e| e.visit_mut(visitor));
        self.func = self.func.map(|f| f.visit_mut(visitor));

        self
    }
}

impl Visit for OnEventStatementNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_on_event_statement(self);

        if let Some(func) = &self.func {
            func.visit(visitor);
        }
    }
}

impl VisitMut for OnEventStatementNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_on_event_statement(self);

        self.func = self.func.map(|f| f.visit_mut(visitor));

        self
    }
}

impl Visit for BlockStatementNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_block_statement(self);

        for node in self.nodes.iter() {
            node.visit(visitor);
        }
    }
}

impl VisitMut for BlockStatementNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_block_statement(self);

        self.nodes = self.nodes.into_iter().map(|n| n.visit_mut(visitor)).collect::<_>();

        self = visitor.visit_block_statement_end(self);

        self
    }
}

impl Visit for LabelDeclarationNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_label_declaration(self);
    }
}

impl VisitMut for LabelDeclarationNode {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        visitor.visit_label_declaration(self)
    }
}

impl Visit for FunctionDeclarationNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_function_declaration(self);
    }
}

impl VisitMut for FunctionDeclarationNode {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        visitor.visit_function_declaration(self)
    }
}

impl Visit for UsedllDeclarationNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_usedll_declaration(self);
    }
}

impl VisitMut for UsedllDeclarationNode {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        visitor.visit_usedll_declaration(self)
    }
}

impl Visit for CommandStatementNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_command_statement(self);

        self.func.visit(visitor);
    }
}

impl VisitMut for CommandStatementNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_command_statement(self);

        self.func = self.func.visit_mut(visitor);

        self
    }
}

impl Visit for McallStatementNode {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_mcall_statement(self);

        self.primary_exp.visit(visitor);

        if let Some(arg) = &self.arg {
            arg.visit(visitor);
        }
    }
}

impl VisitMut for McallStatementNode {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_mcall_statement(self);

        self.primary_exp = self.primary_exp.visit_mut(visitor);
        self.arg = self.arg.map(|a| a.visit_mut(visitor));

        self
    }
}
