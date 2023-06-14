using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class PlatformTagNode : AstNode {
	public string Name { get; }

	public PlatformTagNode(SourceLoc location, string name) {
		Location = location;
		Name = name;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		return children.AsReadOnly();
	}

	public override string ToString() => $"Tag: {Name}";
}
