using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class ReturnStatementNode : StatementNode {
	public ExpressionNode? Expression { get; }

	public ReturnStatementNode(SourceLoc location, ExpressionNode? expression) {
		Location = location;
		Expression = expression;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		if (Expression != null)
			children.Add(Expression);

		return children.AsReadOnly();
	}

	public override string ToString() => "Return";
}
