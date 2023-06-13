namespace Nyapl.Typing.Effects;

public class MetaEffect : Effect {
	public int ID { get; }

	public MetaEffect(int id) {
		ID = id;
	}

	public override string ToString() => $"MetaEffect({ID})";
}
