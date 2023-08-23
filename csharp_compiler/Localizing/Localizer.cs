using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;

using Nyapl.Typing;
using Nyapl.Typing.Types;
using Nyapl.Typing.Effects;

using Nyapl.CodeGen;

namespace Nyapl.Localizing;

public class Localizer {
	Arguments Args { get; }
	Platform platform { get; }

	public Localizer(Arguments args) {
		Args = args;
		platform = linux_x86_64;

		if (Args.Simulate) {
			platform = simulation;
		}
	}

	public static readonly Platform simulation = new() {
		Tags = new[] {"simulator"},
		Intrinsics = new() {
			{ "write_num", new Apply(new Function(new Effect[0].ToList().AsReadOnly(), true), new Typ[] {new Apply(TypeChecker.tuple, new Typ[0]{}.ToList().AsReadOnly()), TypeChecker.i32}.ToList().AsReadOnly()) }
		},
	};

	public static readonly Platform linux_x86_64 = new() {
		Tags = new[] {"linux", "x86_64"},
		Intrinsics = new() {
		},
		CodeGenerator = new CodeGenLinux_x86_64(),
	};

	public LocalizedFileNode Localize(FileNode file) {
		var functions = new List<FunctionNode>(file.Functions);
		var typedefs  = new List<TypeDefNode>(file.TypeDefs);
		foreach (var platformNode in file.Platforms) {
			// let's check all the platform tags
			foreach (var entry in platformNode.Entries) {
				if (entry.Tags.All(t => platform.Tags.Contains(t.Name))) {
					// copy contents of entry into functions and typedefs
					functions.AddRange(entry.Functions);
					typedefs.AddRange(entry.Typedefs);
				}
			}
		}
		return new(file.Location, platform, functions.AsReadOnly(), typedefs.AsReadOnly());
	}

	public readonly struct Platform {
		public readonly string[] Tags { get; init; }
		public readonly Dictionary<string, Typ> Intrinsics { get; init; }
		public readonly ICodeGen CodeGenerator { get; init; }

		public bool HasIntrinsic(string name) {
			return Intrinsics.ContainsKey(name);
		}

		public Typ GetIntrinsic(string name) {
			return Intrinsics[name];
		}
	}
}
