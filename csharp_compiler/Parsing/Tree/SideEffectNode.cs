using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Effects;

namespace Nyapl.Parsing.Tree;

public class SideEffectNode : AstNode {
	public Effect? Effect { get; }
	public string Name { get; }

	public SideEffectNode(SourceLoc location, string name, Effect? effect = null) {
		Location = location;
		Name = name;
		Effect = effect;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		return children.AsReadOnly();
	}

	public override string ToString() => $"SideEffect {Name}{(Effect == null ? "" : $" [{Effect}]")}";
}
