using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;

using Nyapl.Typing;
using Nyapl.Typing.Types;
using Nyapl.Typing.Effects;

namespace Nyapl.Localizing;

public class Localizer {
	Arguments Args { get; }

	public Localizer(Arguments args) {
		Args = args;
	}

	static readonly Platform simulation = new() {
		PointerSize = 8,
		Tags = new[] {"simulator"},
		Intrinsics = new() {
			{ "write_num", new Apply(new Function(new Effect[0].ToList().AsReadOnly()), new Typ[] {new Apply(TypeChecker.tuple, new Typ[] {TypeChecker.i32}.ToList().AsReadOnly()), TypeChecker.i32}.ToList().AsReadOnly()) }
		},
	};

	public LocalizedFileNode Localize(FileNode file) {
		Platform pfInfo = default;
		if (Args.Simulate) {
			pfInfo = simulation;
		}

		var functions = new List<FunctionNode>(file.Functions);
		var typedefs  = new List<TypeDefNode>(file.TypeDefs);
		foreach (var platform in file.Platforms) {
			// let's check all the platform tags
			foreach (var entry in platform.Entries) {
				if (entry.Tags.All(t => pfInfo.Tags.Contains(t.Name))) {
					// copy contents of entry into functions and typedefs
					functions.AddRange(entry.Functions);
					typedefs.AddRange(entry.Typedefs);
				}
			}
		}
		return new(file.Location, pfInfo, functions.AsReadOnly(), typedefs.AsReadOnly());
	}

	public readonly struct Platform {
		public readonly int PointerSize { get; init; }
		public readonly string[] Tags { get; init; }
		public readonly Dictionary<string, Typ> Intrinsics { get; init; }

		public bool HasIntrinsic(string name) {
			return Intrinsics.ContainsKey(name);
		}

		public Typ GetIntrinsic(string name) {
			return Intrinsics[name];
		}
	}
}
