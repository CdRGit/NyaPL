namespace Nyapl.Interpretation;

using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

using Nyapl.Localizing;

public class Interpreter {
	private Value GetValueFromParam(Dictionary<uint, Value> registers, ReadOnlyDictionary<string, IrBlock> functions, IrParam param) {
		switch (param) {
			case IrParam.Int i:
				return new Value.Integer(i.Value);
			case IrParam.Register r:
				return registers[r.Index];
			case IrParam.Function f:
				return new Value.Function(functions.Keys.Skip((int)f.Index).First());
			default:
				throw new Exception($"Can't find value of '{param}' yet");
		}
	}

	private ulong KeepInRange(ushort size, ulong val) {
		ulong mask = ((1ul << (size * 8)) - 1);
		return mask & val;
	}

	public Value Function(ReadOnlyDictionary<string, IrBlock> functions, string name, Value[] args) {
		throw new Exception("TODO!");
/*
		Dictionary<uint, Value> registers = new();

		var current = functions[name];
		var index = 0;

		while (true) {
			var instr = current.Instructions[index++];
			switch (instr.Kind) {
				case IrKind.Return:
					return GetValueFromParam(registers, functions, instr[0]!);
				case IrKind.IntrinsicImpure:
					{
						var idx = (instr[0] as IrParam.Register)!.Index;
						var intrinsic = Localizer.simulation.Intrinsics.Keys.ToArray()[(instr[1] as IrParam.Intrinsic)!.Index];
						var _args = instr.Params.Skip(2).Select(p => GetValueFromParam(registers, functions, p)).ToArray();
						switch (intrinsic) {
							case "write_num":
								Console.WriteLine((_args[0] as Value.Integer)!.Value);
								break;
							default:
								throw new Exception($"Can't interpret '{intrinsic}' yet");
						}
					}
					break;
				case IrKind.Intrinsic:
					{
						var reg = (instr[0] as IrParam.Register)!;
						var idx = reg.Index;
						var intrinsic = (instr[1] as IrParam.IntrinsicOp)!.Kind;
						var _args = instr.Params.Skip(2).Select(p => GetValueFromParam(registers, functions, p)).ToArray();
						switch (intrinsic) {
							case IrOpKind.Add_signed:
								registers[idx] = new Value.Integer(KeepInRange(reg.Size, (ulong)(
									(long)((_args[0] as Value.Integer)!.Value)
									+
									(long)((_args[1] as Value.Integer)!.Value)
								)));
								break;
							case IrOpKind.Subtract_signed:
								registers[idx] = new Value.Integer(KeepInRange(reg.Size, (ulong)(
									(long)((_args[0] as Value.Integer)!.Value)
									-
									(long)((_args[1] as Value.Integer)!.Value)
								)));
								break;
							case IrOpKind.Equal_integer:
								registers[idx] = new Value.Bool(
									(_args[0] as Value.Integer)!.Value
									==
									(_args[1] as Value.Integer)!.Value
								);
								break;
							case IrOpKind.NotEq_integer:
								registers[idx] = new Value.Bool(
									(_args[0] as Value.Integer)!.Value
									!=
									(_args[1] as Value.Integer)!.Value
								);
								break;
							default:
								throw new Exception($"Can't interpret '{intrinsic}' yet");
						}
					}
					break;
				case IrKind.Call:
				case IrKind.CallImpure:
					{
						var idx = (instr[0] as IrParam.Register)!.Index;
						var callee = GetValueFromParam(registers, functions, instr[1]!);
						var _args = instr.Params.Skip(2).Select(p => GetValueFromParam(registers, functions, p)).ToArray();
						registers[idx] = Function(functions, (callee as Value.Function)!.Name, _args);
					}
					break;
				case IrKind.Copy:
				case IrKind.LoadFunction:
					{
						var idx = (instr[0] as IrParam.Register)!.Index;
						registers[idx] = GetValueFromParam(registers, functions, instr[1]!);
					}
					break;
				case IrKind.BranchBool:
					index = 0;
					current = !(GetValueFromParam(registers, functions, instr[0]!) as Value.Bool)!.Value ? (instr[1] as IrParam.Block)!.Blk : (instr[2] as IrParam.Block)!.Blk;
					break;
				case IrKind.BranchAlways:
					index = 0;
					current = (instr[0] as IrParam.Block)!.Blk;
					break;
				case IrKind.LoadArguments:
					for (int i = 0; i < instr.Params.Length; i++) {
						registers[(instr[i] as IrParam.Register)!.Index] = args[i];
					}
					break;
				case IrKind.CreateTuple:
					{
						var idx = (instr[0] as IrParam.Register)!.Index;
						var values = new Dictionary<ulong, Value>();
						for (int i = 1; i < instr.Params.Length; i += 2) {
							var offset = (instr[i+1] as IrParam.Offset)!.Value;
							var val = GetValueFromParam(registers, functions, instr[i]!);
							values[offset] = val;
						}
						registers[idx] = new Value.Tuple(values.AsReadOnly());
					}
					break;
				case IrKind.LoadTupleSection:
					{
						var idx = (instr[0] as IrParam.Register)!.Index;
						var val = GetValueFromParam(registers, functions, instr[1]!);
						var offset = (instr[2] as IrParam.Offset)!.Value;
						registers[idx] = (val as Value.Tuple)!.Values[offset];
					}
					break;
				default:
					throw new Exception($"Can't interpret '{instr}' yet");
			}
		}*/
	}
}

public abstract class Value {
	public sealed class Integer : Value {
		public Integer(ulong v) => Value = v;
		public readonly ulong Value;
		public override string ToString() => $"Int({Value})";
	}
	public sealed class Bool : Value {
		public Bool(bool v) => Value = v;
		public readonly bool Value;
	}
	public sealed class Function : Value {
		public Function(string n) => Name = n;
		public readonly string Name;
	}
	public sealed class Tuple : Value {
		public Tuple(ReadOnlyDictionary<ulong, Value> values) => Values = values;
		public readonly ReadOnlyDictionary<ulong, Value> Values;
	}
}
