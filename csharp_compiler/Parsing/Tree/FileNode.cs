using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class FileNode : AstNode {
	public ReadOnlyCollection<FunctionNode> Functions { get; }
	public ReadOnlyCollection<TypeDefNode> TypeDefs { get; }
	public ReadOnlyCollection<TopLevelPlatformNode> Platforms { get; }

	public FileNode(SourceLoc location, ReadOnlyCollection<FunctionNode> functions, ReadOnlyCollection<TypeDefNode> typeDefs, ReadOnlyCollection<TopLevelPlatformNode> platforms) {
		Location = location;
		Functions = functions;
		TypeDefs = typeDefs;
		Platforms = platforms;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.AddRange(TypeDefs);
		children.AddRange(Functions);
		children.AddRange(Platforms);

		return children.AsReadOnly();
	}

	public override string ToString() => "File";
}
