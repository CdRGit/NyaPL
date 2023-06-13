using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class NamedTypeNode : TypeNode {
	public string Name { get; }

	public NamedTypeNode(SourceLoc location, string name, Typ? type = null) {
		Location = location;
		Name = name;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		return children.AsReadOnly();
	}

	public override string ToString() => $"NamedType {Name}{(Type == null ? "" : $" :{Type}")}";
}
