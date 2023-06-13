using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public abstract class ExpressionNode : AstNode, ITypedNode {
	public Typ? Type { get; set; }
}
