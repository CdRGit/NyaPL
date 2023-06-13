using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing;

namespace Nyapl.Lexing;

public class TokenList : IEnumerable<Token> {
	public Token this[int i] { get => tokens[i]; }

	private ReadOnlyCollection<Token> tokens;

	public TokenList(ReadOnlyCollection<Token> tokens) {
		this.tokens = tokens;
	}

	public IEnumerator<Token> GetEnumerator() => tokens.GetEnumerator();
	IEnumerator IEnumerable.GetEnumerator() => tokens.GetEnumerator();

	public class Context {
		readonly TokenList parent;
		int i = 0;

		public Token Current { get => parent[i]; }

		public Context(TokenList parent) => this.parent = parent;

		public Token Take(TokenKind kind) {
			if (!Match(kind)) throw new ParseError(Current.Location, $"Expected {kind} got {Current.Kind}");
			return Advance();
		}

		public bool Match(TokenKind kind) {
			return Current.Kind == kind;
		}

		public Token Take(KeywordKind kind) {
			if (!Match(kind)) throw new ParseError(Current.Location, $"Expected {TokenKind.Keyword}:{kind} got {Current}");
			return Advance();
		}

		public bool Match(KeywordKind kind) {
			return Current.Kind == TokenKind.Keyword && Current.KeyVal == kind;
		}

		public Token Advance() => parent[i++];
	}
}
