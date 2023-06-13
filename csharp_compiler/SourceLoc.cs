namespace Nyapl;

public readonly struct SourceLoc {
	public string Path { get; }
	public int Col { get; }
	public int Line { get; }

	public SourceLoc(string path) : this(path, 0, 0) {}

	private SourceLoc(string path, int col, int line) {
		Path = path;
		Col = col;
		Line = line;
	}

	public SourceLoc Move(char c) {
		switch (c) {
			case '\t':
				return new(Path, Col + (4 - Col % 4), Line);
			case '\r':
				return new(Path, 0, Line);
			case '\n':
				return new(Path, 0, Line+1);
			default:
				return new(Path, Col+1, Line);
		}
	}

	public override string ToString() {
		return $"{Path}:{Line+1}:{Col+1}";
	}
}
