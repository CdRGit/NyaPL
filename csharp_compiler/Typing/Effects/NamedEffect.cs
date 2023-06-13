namespace Nyapl.Typing.Effects;

public class NamedEffect : Effect {
	public string Name { get; }

	public NamedEffect(string name) {
		Name = name;
	}

	public override string ToString() => $"NamedEffect({Name})";
}
