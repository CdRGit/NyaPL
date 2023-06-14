using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class TopLevelPlatformNode : AstNode {
	public AstListNode<Entry> Entries { get; }

	public TopLevelPlatformNode(SourceLoc location, AstListNode<Entry> entries) {
		Location = location;
		Entries = entries;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		children.Add(Entries);
		return children.AsReadOnly();
	}

	public override string ToString() => $"TopLevelPlatform";

	public class Entry : AstNode {

		public AstListNode<PlatformTagNode> Tags { get; }
		public ReadOnlyCollection<FunctionNode> Functions { get; }
		public ReadOnlyCollection<TypeDefNode> Typedefs { get; }

		public Entry(SourceLoc location, AstListNode<PlatformTagNode> tags, ReadOnlyCollection<FunctionNode> functions, ReadOnlyCollection<TypeDefNode> typedefs) {
			Location = location;
			Tags = tags;
			Functions = functions;
			Typedefs = typedefs;
		}

		public override ReadOnlyCollection<AstNode> GetChildren() {
			List<AstNode> children = new();
			children.Add(Tags);
			children.AddRange(Functions);
			children.AddRange(Typedefs);
			return children.AsReadOnly();
		}

		public override string ToString() => $"Entry";
	}
}
