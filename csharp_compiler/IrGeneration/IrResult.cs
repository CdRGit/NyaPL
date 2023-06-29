using System.Collections.ObjectModel;

using Nyapl.Localizing;

namespace Nyapl.IrGeneration;

public class IrResult {
	public Localizer.Platform Platform { get; }
	public ReadOnlyCollection<IrBlock> Blocks { get; }
	public ReadOnlyDictionary<string, IrBlock> Functions { get; }

	public IrResult(Localizer.Platform platform, ReadOnlyCollection<IrBlock> blocks, ReadOnlyDictionary<string, IrBlock> functions) {
		Platform = platform;
		Blocks = blocks;
		Functions = functions;
	}
}
