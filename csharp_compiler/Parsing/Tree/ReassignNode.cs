using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class ReassignNode : StatementNode {
	public LValueNode LVal { get; }
	public ExpressionNode Expr { get; }

	public ReassignNode(SourceLoc location, LValueNode lVal, ExpressionNode expr) {
		Location = location;
		LVal = lVal;
		Expr = expr;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(LVal);
		children.Add(Expr);

		return children.AsReadOnly();
	}

	public override string ToString() {
		return $"Reassign";
	}
}
