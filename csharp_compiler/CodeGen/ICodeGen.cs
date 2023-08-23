using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public interface ICodeGen {
	public void Generate(string filePath, ReadOnlyDictionary<string, IrBlock> functions);
}
