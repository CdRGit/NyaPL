using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public abstract class TypeNode : AstNode, ITypedNode {
	public Typ? Type { get; set; }
}
