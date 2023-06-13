using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class TypeDefNode : AstNode, ITypedNode {
	public string Name { get; }
	public TypeNode TypeNode { get; }
	public Typ? Type { get; set; }

	public TypeDefNode(SourceLoc location, string name, TypeNode typeNode, Typ? type = null) {
		Location = location;
		Name = name;
		TypeNode = typeNode;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		children.Add(TypeNode);
		return children.AsReadOnly();
	}

	public override string ToString() => $"TypeDef {Name}";
}
