using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class PointerTypeNode : TypeNode {
	public TypeNode BaseType { get; }

	public PointerTypeNode(SourceLoc location, TypeNode baseType, Typ? type = null) {
		Location = location;
		BaseType = baseType;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(BaseType);

		return children.AsReadOnly();
	}

	public override string ToString() => $"PointerType{(Type == null ? "" : $" :{Type}")}";
}
