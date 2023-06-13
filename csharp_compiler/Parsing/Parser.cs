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

	private ExpressionNode ParseAtom(TokenList.Context tokens) {
		// all I know is number & VarLookup
		if (tokens.Match(TokenKind.Integer)) {
			Token token = tokens.Take(TokenKind.Integer);
			return new IntLiteralNode(token.Location, token.IntVal);
		} else if (tokens.Match(TokenKind.Identifier)) {
			Token token = tokens.Take(TokenKind.Identifier);
			return new VarLookupNode(token.Location, token.StrVal);
		} else if (tokens.Match(KeywordKind.Hole)) {
			Token token = tokens.Take(KeywordKind.Hole);
			return new HoleExpressionNode(token.Location);
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

	private ExpressionNode ParseExpression(TokenList.Context tokens) {
		var expr = ParsePrimary(tokens);
		// add
		while (tokens.Match(TokenKind.Plus)) {
			SourceLoc loc = tokens.Take(TokenKind.Plus).Location;
			var rExpr = ParsePrimary(tokens);
			expr = new BinOpNode(loc, expr, BinOpKind.Add, rExpr);
		}
		return expr;
	}

	private StatementNode ParseStatement(TokenList.Context tokens) {
		if (tokens.Match(KeywordKind.Return)) {
			var location = tokens.Take(KeywordKind.Return).Location;
			var expression = ParseExpression(tokens);
			tokens.Take(TokenKind.SemiColon);
			return new ReturnStatementNode(location, expression);
		} else if (tokens.Match(KeywordKind.Let)) {
			SourceLoc location = tokens.Take(KeywordKind.Let).Location;
			string name = tokens.Take(TokenKind.Identifier).StrVal;
			TypeNode type = tokens.Match(TokenKind.Colon) ? ParseTypeTag(tokens) : new TypeHoleNode(tokens.Current.Location);
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

	private TypeNode ParseType(TokenList.Context tokens) {
		if (tokens.Match(KeywordKind.Hole)) return new TypeHoleNode(tokens.Take(KeywordKind.Hole).Location);
		else if (tokens.Match(KeywordKind.Function)) return ParseFunctionType(tokens);

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
