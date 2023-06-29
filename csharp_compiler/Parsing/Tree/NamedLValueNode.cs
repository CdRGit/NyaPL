using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class NamedLValueNode : LValueNode {
	public string Name { get; }

	public NamedLValueNode(SourceLoc location, string name, Typ? type = null) {
		Location = location;
		Name = name;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		return children.AsReadOnly();
	}

	public override string ToString() {
		return $"NamedLValue {Name}{(Type == null ? "" : $" :{Type}")}";
	}
}
