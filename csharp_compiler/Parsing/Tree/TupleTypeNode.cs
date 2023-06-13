using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class TupleTypeNode : TypeNode {
	public AstListNode<TypeNode> Types { get; }

	public TupleTypeNode(SourceLoc location, AstListNode<TypeNode> types, Typ? type = null) {
		Location = location;
		Types = types;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Types);

		return children.AsReadOnly();
	}

	public override string ToString() => $"TupleType{(Type == null ? "" : $" :{Type}")}";
}
