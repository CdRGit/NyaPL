using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class NoopStatementNode : StatementNode {
	public NoopStatementNode(SourceLoc location) {
		Location = location;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		return children.AsReadOnly();
	}

	public override string ToString() => "Nop";
}
