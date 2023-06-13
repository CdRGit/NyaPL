using System;
using System.Collections.ObjectModel;

namespace Nyapl;

public class BundledError : CompileError {
	public ReadOnlyCollection<CompileError> Errors { get; }

	public BundledError(ReadOnlyCollection<CompileError> errors) : base(errors[0].Location, "", "Bundled") {
		Errors = errors;
	}
}
