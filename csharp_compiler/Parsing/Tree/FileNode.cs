using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class FileNode : AstNode {
	public ReadOnlyCollection<FunctionNode> Functions { get; }
	public ReadOnlyCollection<TypeDefNode> TypeDefs { get; }

	public FileNode(SourceLoc location, ReadOnlyCollection<FunctionNode> functions, ReadOnlyCollection<TypeDefNode> typeDefs) {
		Location = location;
		Functions = functions;
		TypeDefs = typeDefs;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.AddRange(TypeDefs);
		children.AddRange(Functions);

		return children.AsReadOnly();
	}

	public override string ToString() => "File";
}
