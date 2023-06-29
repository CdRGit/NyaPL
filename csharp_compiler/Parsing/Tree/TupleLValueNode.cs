using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class TupleLValueNode : LValueNode {
	public AstListNode<LValueNode> Children { get; }

	public TupleLValueNode(SourceLoc location, AstListNode<LValueNode> children, Typ? type = null) {
		Location = location;
		Children = children;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Children);

		return children.AsReadOnly();
	}

	public override string ToString() {
		return $"TupleLValue{(Type == null ? "" : $" :{Type}")}";
	}
}
