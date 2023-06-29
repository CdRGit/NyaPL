using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class DestructureNode : StatementNode {
	public AstListNode<DestructureItemNode> Items { get; }
	public bool Mutable { get; }
	public ExpressionNode Expression { get; }

	public DestructureNode(SourceLoc location, AstListNode<DestructureItemNode> items, bool mutable, ExpressionNode expression) {
		Location = location;
		Items = items;
		Mutable = mutable;
		Expression = expression;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Items);
		children.Add(Expression);

		return children.AsReadOnly();
	}

	public override string ToString() => $"Destructure {(Mutable ? "[Mutable] " : "")}";
}
