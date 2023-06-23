using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public abstract class LValueNode : AstNode, ITypedNode {
	public Typ? Type { get; set; }
}
