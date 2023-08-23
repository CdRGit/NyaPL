using System;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public class CodeGenLinux_x86_64 : ICodeGen {
	public void Generate(string filePath, ReadOnlyDictionary<string, IrBlock> functions) {
		throw new Exception("TODO!");
	}
}
