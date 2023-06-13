using System;

namespace Nyapl;

public class CompileError : Exception {
	public SourceLoc Location { get; }

	public CompileError(SourceLoc location, string message, string stage) : base($"{stage} Error at {location}: {message}") {
		Location = location;
	}
}
