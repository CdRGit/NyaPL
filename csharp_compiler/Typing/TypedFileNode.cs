using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;
using Nyapl.Localizing;

namespace Nyapl.Typing;

public class TypedFileNode : AstNode {
	public Localizer.Platform Platform { get; }
	public ReadOnlyCollection<FunctionNode> Functions { get; }
	public ReadOnlyCollection<TypeDefNode> TypeDefs { get; }
	public TypeChecker.Context Context { get; }

	public TypedFileNode(SourceLoc location, Localizer.Platform platform, ReadOnlyCollection<FunctionNode> functions, ReadOnlyCollection<TypeDefNode> typeDefs, TypeChecker.Context context) {
		Location = location;
		Platform = platform;
		Functions = functions;
		TypeDefs = typeDefs;
		Context = context;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.AddRange(TypeDefs);
		children.AddRange(Functions);

		return children.AsReadOnly();
	}

	public override string ToString() => "TypedFile";
}
