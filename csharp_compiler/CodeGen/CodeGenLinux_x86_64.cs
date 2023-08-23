using System;
using System.IO;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public class CodeGenLinux_x86_64 : ICodeGen {
	public void Generate(string filePath, ReadOnlyDictionary<string, IrBlock> functions) {
		using (StreamWriter asmWriter = new($"{filePath}.asm")) {
			asmWriter.WriteLine(".text");
			asmWriter.WriteLine(".global _start");
		}
		throw new Exception("TODO!");
	}
}
