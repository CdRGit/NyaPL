using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class DestructureNode : StatementNode {
	public AstListNode<DestructureItemNode> Names { get; }
	public ExpressionNode Expression { get; }

	public DestructureNode(SourceLoc location, AstListNode<DestructureItemNode> names, ExpressionNode expression) {
		Location = location;
		Names = names;
		Expression = expression;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Names);
		children.Add(Expression);

		return children.AsReadOnly();
	}

	public override string ToString() => $"Destructure";
}
