using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

interface ITypedNode {
	public Typ? Type { get; set; }
}
