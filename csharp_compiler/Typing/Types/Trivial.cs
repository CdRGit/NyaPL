namespace Nyapl.Typing.Types;

public class Trivial : Typ {
	public string Name { get; }

	public Trivial(string name) {
		Name = name;
	}

	public override string ToString() => $"Trivial({Name})";
}
