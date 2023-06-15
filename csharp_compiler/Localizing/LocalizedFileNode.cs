using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;

namespace Nyapl.Localizing;

public class LocalizedFileNode : AstNode {
	public Localizer.Platform Platform { get; }
	public ReadOnlyCollection<FunctionNode> Functions { get; }
	public ReadOnlyCollection<TypeDefNode> TypeDefs { get; }

	public LocalizedFileNode(SourceLoc location, Localizer.Platform platform, ReadOnlyCollection<FunctionNode> functions, ReadOnlyCollection<TypeDefNode> typeDefs) {
		Location = location;
		Platform = platform;
		Functions = functions;
		TypeDefs = typeDefs;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.AddRange(TypeDefs);
		children.AddRange(Functions);

		return children.AsReadOnly();
	}

	public override string ToString() => "LocalizedFile";
}
