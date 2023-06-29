using System.Collections.ObjectModel;
using System.Collections.Generic;

namespace Nyapl.IrGeneration;

public class IrBlock {
	List<IrInstr> instructions = new();
	public ReadOnlyCollection<IrInstr> Instructions { get => instructions.AsReadOnly(); }
	public int ID { get; }

	public IrBlock(int id) {
		ID = id;
	}

	public void AddInstr(IrInstr instruction) {
		instructions.Add(instruction);
	}
}
