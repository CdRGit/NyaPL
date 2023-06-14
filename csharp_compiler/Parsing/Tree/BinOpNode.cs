using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class BinOpNode : ExpressionNode {
	public ExpressionNode LExpr { get; }
	public BinOpKind OP { get; }
	public ExpressionNode RExpr { get; }

	public BinOpNode(SourceLoc location, ExpressionNode lExpr, BinOpKind op, ExpressionNode rExpr, Typ? type = null) {
		Location = location;
		LExpr = lExpr;
		OP = op;
		RExpr = rExpr;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		var children = new List<AstNode>();

		children.Add(LExpr);
		children.Add(RExpr);

		return children.AsReadOnly();
	}

	public override string ToString() => $"BinOp({OP}){(Type == null ? "" : $" :{Type}")}";
}

public enum BinOpKind {
	Add,
	Subtract,

	Multiply,
	Divide,
	Modulo,

	Equal,
	NotEq,
}
