using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class WhileStatementNode : StatementNode {
	public ExpressionNode Expr { get; }
	public AstListNode<StatementNode> Body { get; }

	public WhileStatementNode(SourceLoc location, ExpressionNode expr, AstListNode<StatementNode> body) {
		Location = location;
		Expr = expr;
		Body = body;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Expr);
		children.Add(Body);

		return children.AsReadOnly();
	}

	public override string ToString() => "While";
}
