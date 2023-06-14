using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Lexing;

public class Lexer {
	private Dictionary<string, KeywordKind> keywords = new() {
		{"_",        KeywordKind.Hole},
		{"fn",       KeywordKind.Function},
		{"if",       KeywordKind.If},
		{"elif",     KeywordKind.ElIf},
		{"else",     KeywordKind.Else},
		{"let",      KeywordKind.Let},
		{"typedef",  KeywordKind.TypeDef},
		{"return",   KeywordKind.Return},
		{"platform", KeywordKind.Platform},
		{"intrinsic",KeywordKind.Intrinsic},
		{"true",     KeywordKind.True},
		{"false",    KeywordKind.False},
	};

	private Dictionary<char, TokenKind> basics = new() {
		{'(', TokenKind.LParen},
		{')', TokenKind.RParen},
		{'{', TokenKind.LCurly},
		{'}', TokenKind.RCurly},
		{'[', TokenKind.LSquare},
		{']', TokenKind.RSquare},

		{':', TokenKind.Colon},
		{',', TokenKind.Comma},
		{';', TokenKind.SemiColon},

		{'+', TokenKind.Plus},
		{'-', TokenKind.Minus},

		{'/', TokenKind.Slash},
		{'*', TokenKind.Star},
		{'%', TokenKind.Percent},
	};

	private Dictionary<char, (char next, TokenKind nextFound, TokenKind normal)> multiCharBasics = new() {
		{'=', ('=', TokenKind.EqEq, TokenKind.Assign)},
		{'!', ('=', TokenKind.BangEq, TokenKind.Bang)},
	};

	public TokenList Lex(string sourceFile, string sourceCode) {
		List<Token> tokens = new();

		var loc = new SourceLoc(sourceFile);

		var i = 0;

		char MoveNext() {
			char c = sourceCode[i++];
			loc = loc.Move(c);
			return c;
		}

		while (i < sourceCode.Length) {
			char c = sourceCode[i];

			if (char.IsAsciiLetter(c) || c == '_') {
				string s = "";
				SourceLoc start = loc;
				while (char.IsAsciiLetterOrDigit(c) || c == '_') {
					s += c;
					MoveNext();
					c = sourceCode[i];
				}
				if (keywords.ContainsKey(s))
					tokens.Add(Token.Keyword(start, keywords[s]));
				else
					tokens.Add(Token.Identifier(start, s));
				continue;
			} else if (char.IsAsciiDigit(c)) {
				string s = "";
				SourceLoc start = loc;
				while (char.IsAsciiDigit(c) || c == '_') {
					if (c != '_')
						s += c;
					MoveNext();
					c = sourceCode[i];
				}
				tokens.Add(Token.Integer(start, ulong.Parse(s)));
				continue;
			} else if (char.IsWhiteSpace(c)) {
				MoveNext();
				continue;
			} else if (c == '/' && sourceCode[i + 1] == '/') {
				// single line comment
				MoveNext();
				MoveNext(); // read in comment start
				while (MoveNext() != '\n');
				continue;
			} else if (c == '/' && sourceCode[i + 1] == '*') {
				// multi line comment
				MoveNext();
				MoveNext(); // read in comment start
				while (true) {
					if (MoveNext() == '*' && sourceCode[i] == '/') break;
				}
				MoveNext();
				continue;
			} else if (basics.ContainsKey(c)) {
				SourceLoc start = loc;
				MoveNext();
				tokens.Add(Token.Basic(start, basics[c]));
				continue;
			} else if (multiCharBasics.ContainsKey(c)) {
				SourceLoc start = loc;
				var multiCharData = multiCharBasics[c];
				MoveNext();
				if (sourceCode[i] == multiCharData.next) {
					MoveNext();
					tokens.Add(Token.Basic(start, multiCharData.nextFound));
				} else {
					tokens.Add(Token.Basic(start, multiCharData.normal));
				}
				continue;
			}

			throw new LexError(loc, $"Unexpected character: {c}");
		}

		tokens.Add(Token.EOF(loc));

		return new(tokens.AsReadOnly());
	}
}

public readonly struct Token {
	public SourceLoc Location { get; }
	public TokenKind Kind     { get; }

	public string StrVal { get => sVal ?? throw new Exception("no StrVal"); }
	private readonly string? sVal;

	public ulong IntVal { get => iVal ?? throw new Exception("no IntVal"); }
	private readonly ulong? iVal;

	public KeywordKind KeyVal { get => kVal ?? throw new Exception("no KeyVal"); }
	private readonly KeywordKind? kVal;

	private Token(SourceLoc location, TokenKind kind, string? sVal = null, ulong? iVal = null, KeywordKind? kVal = null) {
		Location = location;
		Kind = kind;
		this.sVal = sVal;
		this.iVal = iVal;
		this.kVal = kVal;
	}

	public override string ToString() {
		if (sVal is not null) {
			return $"{Kind,12} : {sVal}";
		} else if (iVal is not null) {
			return $"{Kind,12} : {iVal}";
		} else if (kVal is not null) {
			return $"{Kind,12} : {kVal}";
		} else {
			return $"{Kind,12}";
		}
	}

	public static Token Basic(SourceLoc location, TokenKind kind)      => new(location, kind);
	public static Token EOF(SourceLoc location)                        => new(location, TokenKind.EOF);
	public static Token Identifier(SourceLoc location, string val)     => new(location, TokenKind.Identifier, sVal: val);
	public static Token Integer(SourceLoc location, ulong val)         => new(location, TokenKind.Integer,    iVal: val);
	public static Token Keyword(SourceLoc location, KeywordKind kind)  => new(location, TokenKind.Keyword,    kVal: kind);
}

public enum KeywordKind {
	If,
	ElIf,
	Else,

	Platform,
	Intrinsic,
	Function,
	TypeDef,
	Let,
	Return,

	False,
	True,

	Hole,
}

public enum TokenKind {
	Identifier,
	Keyword,

	Integer,

	LParen,
	RParen,
	LCurly,
	RCurly,
	LSquare,
	RSquare,

	EqEq,
	BangEq,

	Assign,
	Bang,

	Colon,
	SemiColon,
	Comma,

	Plus,
	Minus,
	Slash,
	Star,
	Percent,

	EOF,
}
