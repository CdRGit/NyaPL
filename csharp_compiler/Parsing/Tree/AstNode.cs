using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public abstract class AstNode {
	public SourceLoc Location { get; protected init; }

	public abstract ReadOnlyCollection<AstNode> GetChildren();
}
