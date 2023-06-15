using System.Collections.Generic;

using Nyapl.Parsing.Tree;

using Nyapl.Localizing;

namespace Nyapl.IrGeneration;

public class IrGenerator {
	public IEnumerable<IrInstr> Generate(FunctionNode function) {
		yield break;
	}

	public IrList Generate(LocalizedFileNode file) {
		List<IrInstr> instructions = new();

		Dictionary<string, int> functions = new();

		foreach(var function in file.Functions) {
			int fnIdx = instructions.Count;
			instructions.AddRange(Generate(function));
			functions[function.Name] = fnIdx;
		}

		return new(file.Platform, instructions.AsReadOnly(), functions.AsReadOnly());
	}
}
