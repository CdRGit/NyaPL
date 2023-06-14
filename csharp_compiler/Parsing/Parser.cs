using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Lexing;
using Nyapl.Parsing.Tree;

namespace Nyapl.Parsing;

public class Parser {
	private AstListNode<T> ParseList<T>(TokenList.Context tokens, TokenKind open, TokenKind close, TokenKind? separator, Func<TokenList.Context, T> element) where T : AstNode {
		List<T> elements = new();

		SourceLoc location = tokens.Take(open).Location;
		while (!tokens.Match(close)) {
			elements.Add(element(tokens));
			if (separator != null && tokens.Match((TokenKind)separator!)) tokens.Take((TokenKind)separator!);
			if (tokens.Match(close)) break;
		}
		tokens.Take(close);

		return new(location, elements.AsReadOnly());
	}

	private AstListNode<T> ParseOptionalList<T>(TokenList.Context tokens, TokenKind open, TokenKind close, TokenKind? separator, Func<TokenList.Context, T> element) where T : AstNode {
		if (tokens.Match(open)) return ParseList(tokens, open, close, separator, element);
		return new(tokens.Current.Location, new ReadOnlyCollection<T>(new List<T>()));
	}

	private FunctionNode ParseFunction(TokenList.Context tokens) {
		SourceLoc location = tokens.Take(KeywordKind.Function).Location;
		AstListNode<SideEffectNode> sideEffects = ParseOptionalList(tokens, TokenKind.LSquare, TokenKind.RSquare, null, ParseSideEffect);
		string name = tokens.Take(TokenKind.Identifier).StrVal;
		AstListNode<ParameterNode> parameters = ParseList(tokens, TokenKind.LParen, TokenKind.RParen, TokenKind.Comma, ParseParameter);
		TypeNode returnType = ParseTypeTag(tokens);
		AstListNode<StatementNode> body = ParseList(tokens, TokenKind.LCurly, TokenKind.RCurly, null, ParseStatement);

		return new(location, name, sideEffects, parameters, returnType, body);
	}

	private TypeNode ParseOptionalTypeTag(TokenList.Context tokens) {
		return tokens.Match(TokenKind.Colon) ? ParseTypeTag(tokens) : new TypeHoleNode(tokens.Current.Location);
	}

	private ExpressionNode ParseAtom(TokenList.Context tokens) {
		if (tokens.Match(KeywordKind.True)) {
			return new BoolLiteralNode(tokens.Take(KeywordKind.True).Location, true);
		} else if (tokens.Match(KeywordKind.False)) {
			return new BoolLiteralNode(tokens.Take(KeywordKind.False).Location, false);
		} else if (tokens.Match(TokenKind.Integer)) {
			Token token = tokens.Take(TokenKind.Integer);
			return new IntLiteralNode(token.Location, token.IntVal);
		} else if (tokens.Match(TokenKind.Identifier)) {
			Token token = tokens.Take(TokenKind.Identifier);
			return new VarLookupNode(token.Location, token.StrVal);
		} else if (tokens.Match(KeywordKind.Hole)) {
			Token token = tokens.Take(KeywordKind.Hole);
			return new HoleExpressionNode(token.Location);
		} else if (tokens.Match(TokenKind.LParen)) {
			var loc = tokens.Current.Location;
			var list = ParseList(tokens, TokenKind.LParen, TokenKind.RParen, TokenKind.Comma, ParseExpression);
			if (list.Children.Count == 1) {
				return list.Children[0];
			}
			return new TupleNode(loc, list);
		}
		throw new Exception($"ParseAtom(): atom starting with ${tokens.Current} not implemented yet");
	}

	private ExpressionNode ParsePrimary(TokenList.Context tokens) {
		var expr = ParseAtom(tokens);
		// call
		while (tokens.Match(TokenKind.LParen)) {
			SourceLoc loc = tokens.Current.Location;
			var arguments = ParseList(tokens, TokenKind.LParen, TokenKind.RParen, TokenKind.Comma, ParseExpression);
			expr =  new CallNode(loc, expr, arguments);
		}
		return expr;
	}

	readonly Dictionary<TokenKind, UnOpKind> unary = new() {
		{TokenKind.Bang,  UnOpKind.Not},
		{TokenKind.Plus,  UnOpKind.Positive},
		{TokenKind.Minus, UnOpKind.Negative},
	};

	private ExpressionNode ParseUnary(TokenList.Context tokens) {
		if (unary.ContainsKey(tokens.Current.Kind)) {
			var kind = tokens.Current.Kind;
			var loc  = tokens.Take(kind).Location;
			var expr = ParsePrimary(tokens);
			return new UnOpNode(loc, expr, unary[kind]);
		}
		return ParsePrimary(tokens);
	}

	private ExpressionNode ParseBinary(TokenList.Context tokens, Func<TokenList.Context, ExpressionNode> childrenParser, Dictionary<TokenKind, BinOpKind> operators) {
		var expr = childrenParser(tokens);
		while (operators.ContainsKey(tokens.Current.Kind)) {
			var kind = tokens.Current.Kind;
			var loc = tokens.Take(kind).Location;
			var rExpr = childrenParser(tokens);
			expr = new BinOpNode(loc, expr, operators[kind], rExpr);
		}
		return expr;
	}

	readonly Dictionary<TokenKind, BinOpKind> additive = new() {
		{TokenKind.Plus,  BinOpKind.Add},
		{TokenKind.Minus, BinOpKind.Subtract},
	};
	readonly Dictionary<TokenKind, BinOpKind> multiplicative = new() {
		{TokenKind.Star,    BinOpKind.Multiply},
		{TokenKind.Slash,   BinOpKind.Divide},
		{TokenKind.Percent, BinOpKind.Modulo},
	};
	readonly Dictionary<TokenKind, BinOpKind> equality = new() {
		{TokenKind.EqEq,   BinOpKind.Equal},
		{TokenKind.BangEq, BinOpKind.NotEq},
	};
	private ExpressionNode ParseExpression(TokenList.Context tokens) {
		return ParseBinary(tokens, t => ParseBinary(t, t => ParseBinary(t, ParseUnary, multiplicative), additive), equality);
	}

	private DestructureItemNode ParseDestructureItem(TokenList.Context tokens) {
		if (tokens.Match(TokenKind.Identifier)) {
			var tok = tokens.Take(TokenKind.Identifier);
			var type = ParseOptionalTypeTag(tokens);
			return new NamedDestructureNode(tok.Location, tok.StrVal, type);
		}
		else if (tokens.Match(KeywordKind.Hole)) {
			var tok = tokens.Take(KeywordKind.Hole);
			return new HoleDestructureNode(tok.Location);
		}
		throw new Exception($"ParseStatement(): statement starting with ${tokens.Current} not implemented yet");
	}

	private StatementNode ParseStatement(TokenList.Context tokens) {
		if (tokens.Match(KeywordKind.Return)) {
			var location = tokens.Take(KeywordKind.Return).Location;
			var expression = ParseExpression(tokens);
			tokens.Take(TokenKind.SemiColon);
			return new ReturnStatementNode(location, expression);
		} else if (tokens.Match(KeywordKind.Let)) {
			// either we're doing a vardeclare or a destructure
			SourceLoc location = tokens.Take(KeywordKind.Let).Location;
			if (tokens.Match(TokenKind.LParen)) {
				// DESTRUCTURE
				var names = ParseList(tokens, TokenKind.LParen, TokenKind.RParen, TokenKind.Comma, ParseDestructureItem);
				if (names.Children.Count == 1) throw new ParseError(location, "You cannot destructure 1-tuples as they do not exist");
				if (names.Children.Count == 0) throw new ParseError(location, "You cannot destructure 0-tuples, why would you even want to do that? that does nothing!");
				tokens.Take(TokenKind.Assign);
				var val = ParseExpression(tokens);
				tokens.Take(TokenKind.SemiColon);
				return new DestructureNode(location, names, val);
			}
			string name = tokens.Take(TokenKind.Identifier).StrVal;
			TypeNode type = ParseOptionalTypeTag(tokens);
			tokens.Take(TokenKind.Assign);
			ExpressionNode expression = ParseExpression(tokens);
			tokens.Take(TokenKind.SemiColon);
			return new DeclareVarNode(location, name, type, expression);
		} else if (tokens.Match(KeywordKind.Function)) {
			return ParseFunction(tokens);
		}

		throw new Exception($"ParseStatement(): statement starting with ${tokens.Current} not implemented yet");
	}

	private TypeNode ParseTypeTag(TokenList.Context tokens) {
		tokens.Take(TokenKind.Colon);
		return ParseType(tokens);
	}

	private FunctionTypeNode ParseFunctionType(TokenList.Context tokens) {
		var function = tokens.Take(KeywordKind.Function);
		var effects = ParseOptionalList(tokens, TokenKind.LSquare, TokenKind.RSquare, null, ParseSideEffect);
		var parameters = ParseList(tokens, TokenKind.LParen, TokenKind.RParen, TokenKind.Comma, ParseType);
		var returnType = ParseTypeTag(tokens);
		return new(function.Location, effects, parameters, returnType);
	}

	private TupleTypeNode ParseTupleType(TokenList.Context tokens) {
		var location = tokens.Current.Location;
		var types = ParseList(tokens, TokenKind.LParen, TokenKind.RParen, TokenKind.Comma, ParseType);
		if (types.Children.Count == 1) throw new ParseError(location, "1-tuples are not permitted");
		return new(location, types);
	}

	private TypeNode ParseType(TokenList.Context tokens) {
		if (tokens.Match(KeywordKind.Hole)) return new TypeHoleNode(tokens.Take(KeywordKind.Hole).Location);
		else if (tokens.Match(KeywordKind.Function)) return ParseFunctionType(tokens);
		else if (tokens.Match(TokenKind.LParen)) return ParseTupleType(tokens);

		Token tok = tokens.Take(TokenKind.Identifier);
		return new NamedTypeNode(tok.Location, tok.StrVal);
	}

	private ParameterNode ParseParameter(TokenList.Context tokens) {
		Token name = tokens.Take(TokenKind.Identifier);
		var type = ParseTypeTag(tokens);

		return new(name.Location, name.StrVal, type);
	}

	private SideEffectNode ParseSideEffect(TokenList.Context tokens) {
		Token tok = tokens.Take(TokenKind.Identifier);
		return new(tok.Location, tok.StrVal);
	}

	private TypeDefNode ParseTypeDef(TokenList.Context tokens) {
		SourceLoc start = tokens.Take(KeywordKind.TypeDef).Location;
		Token name = tokens.Take(TokenKind.Identifier);
		tokens.Take(TokenKind.Assign);
		TypeNode type = ParseType(tokens);
		tokens.Take(TokenKind.SemiColon);
		return new(start, name.StrVal, type);
	}

	public FileNode Parse(TokenList tokenList) {
		TokenList.Context tokens = new(tokenList);
		SourceLoc start = tokens.Current.Location;

		List<FunctionNode> functions = new();
		List<TypeDefNode> typedefs = new();

		while (!tokens.Match(TokenKind.EOF)) {
			if (tokens.Match(KeywordKind.Function)) functions.Add(ParseFunction(tokens));
			else if (tokens.Match(KeywordKind.TypeDef)) typedefs.Add(ParseTypeDef(tokens));
			else tokens.Take(KeywordKind.Function);
		}

		tokens.Take(TokenKind.EOF);
		return new(start, functions.AsReadOnly(), typedefs.AsReadOnly());
	}
}
