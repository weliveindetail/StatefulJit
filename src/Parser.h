#pragma once

#include <map>
#include <memory>

#include "Globals.h"
#include "Lexer.h"
#include "AST.h"

// ----------------------------------------------------------------------------

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

static std::unique_ptr<ExprAST> ParseExpression();

// ----------------------------------------------------------------------------

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() 
{
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;

  return TokPrec;
}

// ----------------------------------------------------------------------------

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() 
{
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

// ----------------------------------------------------------------------------

/// identifierexpr ::= identifier
static std::unique_ptr<ExprAST> ParseIdentifierExpr() 
{
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  return nullptr;
}

// ----------------------------------------------------------------------------

/// vardef ::= ('double'|'int') identifier ('=' expression)?
static std::unique_ptr<ExprAST> ParseVarDefinitionExpr()
{
  VarDefinitionExprAST::Types type;

  switch (CurTok)
  {
    case tok_double: type = VarDefinitionExprAST::Types::Double; break;
    case tok_int: type = VarDefinitionExprAST::Types::Int; break;
    default:
      return Error("unknown token when expecting a type specifier");
  }

  getNextToken(); // eat the type specifier
  if (CurTok != tok_identifier)
    return Error("expected identifier after a type specifier");

  std::string name = IdentifierStr;
  getNextToken(); // eat identifier

  // read the optional initializer
  std::unique_ptr<ExprAST> init = nullptr;
  if (CurTok == '=') {
    getNextToken(); // eat the '='

    init = ParseExpression();
    if (!init)
      return nullptr;
  }

  return std::make_unique<VarDefinitionExprAST>(type, std::move(name), std::move(init));
}

// ----------------------------------------------------------------------------

/// varexpr ::= 'def' vardef (',' vardef)* 'run' expression
static std::unique_ptr<ExprAST> ParseVarSectionExpr()
{
  getNextToken(); // eat the def

  std::vector<std::unique_ptr<ExprAST>> VarDefs;

  while (1) 
  {
    std::unique_ptr<ExprAST> varDef = ParseVarDefinitionExpr();
    VarDefs.push_back(std::move(varDef));

    // End of var list, exit loop
    if (CurTok != ',')
      break;
    getNextToken(); // eat the ','

    if (!(CurTok == tok_double || CurTok == tok_int))
      return Error("expected another type specifier after ','");
  }

  // At this point, we have to have 'run'
  if (CurTok != tok_execute)
    return Error("expected 'run' keyword after 'def'");
  getNextToken(); // eat 'run'

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarSectionExprAST>(std::move(VarDefs), std::move(Body));
}

// ----------------------------------------------------------------------------

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= varexpr
static std::unique_ptr<ExprAST> ParsePrimary() 
{
  switch (CurTok) 
  {
    case tok_identifier:
      return ParseIdentifierExpr();
    case tok_number:
      return ParseNumberExpr();
    case tok_variables:
      return ParseVarSectionExpr();
    default:
      return Error("unknown token when expecting an expression");
  }
}

// ----------------------------------------------------------------------------

/// binoprhs ::= ('+' unary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, 
                                              std::unique_ptr<ExprAST> LHS) 
{
  // If this is a binop, find its precedence.
  while (1) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

                    // Parse the expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
      std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

// ----------------------------------------------------------------------------

/// expression ::= unary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() 
{
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

// ----------------------------------------------------------------------------

/// toplevelexpr ::= expression
static std::unique_ptr<TopLevelExprAST> ParseTopLevelExpr() 
{
  if (auto E = ParseExpression()) {
    // Make an anonymous top-level expression.
    return std::make_unique<TopLevelExprAST>(std::move(E));
  }
  return nullptr;
}
