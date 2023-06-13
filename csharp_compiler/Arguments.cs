using System.Collections.Generic;
using System.Linq;
using System.IO;

namespace Nyapl;

public readonly struct Arguments {
	public Arguments(string file, string outFile, bool simulate) {
		File = Path.GetFullPath(file);
		OutFile = outFile;
		Simulate = simulate;
	}

	public string File { get; }
	public string OutFile { get; }
	public bool Simulate { get; }

	public override string ToString() {
		return $"File: {File}, Out: {OutFile}{(Simulate ? ", Simulating instead of compiling" : "")}";
	}
}
