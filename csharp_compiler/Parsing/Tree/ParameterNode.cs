using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class ParameterNode : AstNode {
	public string Name { get; }
	public TypeNode Type { get; }

	public ParameterNode(SourceLoc location, string name, TypeNode type) {
		Location = location;
		Name = name;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Type);

		return children.AsReadOnly();
	}

	public override string ToString() => $"Parameter {Name}";
}
