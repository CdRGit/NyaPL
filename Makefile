.PHONY: csharp_compiler
.PHONY: rust_bootstrap

rust_bootstrap:
	cargo run -- test.nya

csharp_compiler:
	dotnet run --project csharp_compiler test.nya -d
