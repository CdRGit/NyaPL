namespace Nyapl.Lexing;

public class LexError : CompileError {
	public LexError(SourceLoc location, string message) : base(location, message, "Lexing") {

	}
}
