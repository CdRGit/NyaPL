using System.Collections.ObjectModel;

using Nyapl.Localizing;

namespace Nyapl.IrGeneration;

public class IrList {
	public Localizer.Platform Platform { get; }
	public ReadOnlyCollection<IrInstr> Instructions { get; }
	public ReadOnlyDictionary<string, int> Functions { get; }

	public IrList(Localizer.Platform platform, ReadOnlyCollection<IrInstr> instructions, ReadOnlyDictionary<string, int> functions) {
		Platform = platform;
		Instructions = instructions;
		Functions = functions;
	}
}
