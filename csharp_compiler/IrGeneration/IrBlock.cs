using System.Collections.ObjectModel;
using System.Collections.Generic;

namespace Nyapl.IrGeneration;

public class IrBlock {
	bool hasReturn;
	public bool HasReturn { get => hasReturn; }

	public bool InstructionComplete { get; private set; }
	public void MarkInstructionComplete() => InstructionComplete = true;

	private Dictionary<string, IrParam.Register> locals = new();
	public ReadOnlyDictionary<string, IrParam.Register> Locals => locals.AsReadOnly();
	public void SetLocals(ReadOnlyDictionary<string, IrParam.Register> newLocals) {
		foreach (var local in newLocals) {
			locals.Add(local.Key, local.Value);
		}
	}

	List<IrInstr> instructions = new();
	public ReadOnlyCollection<IrInstr> Instructions { get => instructions.AsReadOnly(); }
	List<IrBlock> incoming = new();
	List<(string label, IrBlock node)> outgoing = new();
	public ReadOnlyCollection<(string label, IrBlock node)> Outgoing { get => outgoing.AsReadOnly(); }

	public int ID { get; }

	public IrBlock(int id) {
		ID = id;
		hasReturn = false;
	}

	public void AddInstr(IrInstr instruction) {
		if (HasReturn) // a return was already added, no more instructions can be added to a terminal node
			return;
		if (instruction.Kind == IrInstr.IrKind.Return) hasReturn = true;
		instructions.Add(instruction);
	}

	public void AddConnection(IrBlock connectTo, string name = "") {
		if (HasReturn) // a return was already added, this is a terminal node
			return;
		outgoing.Add((name, connectTo));
		connectTo.AddIncoming(this);
	}

	private void AddIncoming(IrBlock connectFrom) {
		incoming.Add(connectFrom);
	}
}
