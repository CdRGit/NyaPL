using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public abstract class DestructureItemNode : AstNode, ITypedNode {
	public Typ? Type { get; set; }
}
