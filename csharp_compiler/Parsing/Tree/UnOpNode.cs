using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class UnOpNode : ExpressionNode {
	public ExpressionNode Expr { get; }
	public UnOpKind OP { get; }

	public UnOpNode(SourceLoc location, ExpressionNode expr, UnOpKind op, Typ? type = null) {
		Location = location;
		Expr = expr;
		OP = op;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		var children = new List<AstNode>();

		children.Add(Expr);

		return children.AsReadOnly();
	}

	public override string ToString() => $"UnOp({OP}){(Type == null ? "" : $" :{Type}")}";
}

public enum UnOpKind {
	Not,

	Positive,
	Negative,
}
