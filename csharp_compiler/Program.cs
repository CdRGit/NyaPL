using System.Linq;
using System.Collections.Generic;
using System;
using System.IO;

namespace Nyapl;

class Program {
	static void Main(string[] strArgs) {
		var args = ParseArgs(strArgs);
		Console.WriteLine(args);

		var compiler = new Compiler(args);
		compiler.Compile();
	}

	enum ParseArgsState {
		File,
		OutFile
	}

	static Arguments ParseArgs(string[] args) {
		ParseArgsState state = ParseArgsState.File;
		string file = "";
		string? outFile = null;
		bool simulate = false;

		foreach (var arg in args) {
			if (arg.StartsWith('-')) {
				// flag of some kind?
				switch (arg) {
					case "-o":
						state = ParseArgsState.OutFile;
						continue;
					case "-s":
						if (simulate) {
							throw new ArgumentException("Already simulating, yet another attempt was included");
						}
						simulate = true;
						continue;

					default:
						throw new ArgumentException($"Unknown flag: {arg}");
				}
			}

			switch (state) {
				case ParseArgsState.File:
					if (!string.IsNullOrEmpty(file)) throw new ArgumentException($"Input file is already set (to {file}), yet an attempt to set it to {arg} was included");
					file = arg;
					break;
				case ParseArgsState.OutFile:
					if (outFile != null) throw new ArgumentException($"Out file is already set (to {outFile}), yet an attempt to set it to {arg} was included");
					outFile = arg;
					state = ParseArgsState.File;
					break;
			}
		}
		return new Arguments(file, outFile ?? Path.ChangeExtension(file, "out"), simulate);
	}
}
