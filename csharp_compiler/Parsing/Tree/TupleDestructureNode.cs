using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class TupleDestructureNode : DestructureItemNode {
	public AstListNode<DestructureItemNode> Children { get; }

	public TupleDestructureNode(SourceLoc location, AstListNode<DestructureItemNode> children, Typ? type = null) {
		Location = location;
		Children = children;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Children);

		return children.AsReadOnly();
	}

	public override string ToString() => $"TupleDestructure{(Type == null ? "" : $":{Type}")}";
}
