namespace Nyapl.Typing.Types;

public class Meta : Typ {
	public int ID { get; }

	public Meta(int id) {
		ID = id;
	}

	public override string ToString() => $"Meta({ID})";
}
