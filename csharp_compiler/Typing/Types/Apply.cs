using System.Collections.ObjectModel;

namespace Nyapl.Typing.Types;

public class Apply : Typ {
	public Typ BaseType { get; }
	public ReadOnlyCollection<Typ> ParameterTypes { get; }

	public Apply(Typ baseType, ReadOnlyCollection<Typ> parameterTypes) {
		BaseType = baseType;
		ParameterTypes = parameterTypes;
	}

	public override string ToString() => $"Apply({BaseType}, [{string.Join(", ", ParameterTypes)}])";
}
