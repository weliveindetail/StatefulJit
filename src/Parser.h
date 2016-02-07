#pragma once

#include <map>
#include <memory>
#include <functional>

#include "Globals.h"
#include "Lexer.h"
#include "AST.h"

// ----------------------------------------------------------------------------

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

// Workaround yet another MSVC bug:
// https://connect.microsoft.com/VisualStudio/feedback/details/1298009
using ResolveType_f = std::function<TypeDefinitionExprAST*(std::string)>;
static ResolveType_f resolveTypeHack;

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
  auto Result = NumVal;
  getNextToken(); // consume the number

  return std::make_unique<NumberExprAST>(Result);
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
  if (CurTok != tok_identifier)
    return Error("expected identifier for variable type");

  TypeDefinitionExprAST* type_rawptr = resolveTypeHack(IdentifierStr);

  if (!type_rawptr)
  {
    std::string msg = "unknown type '" + IdentifierStr + "'";
    return Error(msg.c_str());
  }

  getNextToken(); // eat the type name

  if (CurTok != tok_identifier)
    return Error("expected identifier for variable name");

  std::string name = IdentifierStr;
  getNextToken(); // eat the variable name

  // read the optional initializer
  std::unique_ptr<ExprAST> init = nullptr;
  if (CurTok == '=') {
    getNextToken(); // eat the '='

    init = ParseExpression();
    if (!init)
      return nullptr;
  }

  return std::make_unique<VarDefinitionExprAST>(
    std::move(name), type_rawptr, std::move(init));
}

// ----------------------------------------------------------------------------

/// memdef ::= ('double'|'int') identifier
static std::unique_ptr<ExprAST> ParseCompoundMemberDefinitionExpr()
{
  if (CurTok != tok_identifier)
    return Error("expected identifier for member type");

  TypeDefinitionExprAST* type_rawptr = resolveTypeHack(IdentifierStr);

  if (!type_rawptr)
  {
    std::string msg = "unknown type '" + IdentifierStr + "'";
    return Error(msg.c_str());
  }

  getNextToken(); // eat identifier
  if (CurTok != tok_identifier)
    return Error("expected identifier for member name");

  std::string name = IdentifierStr;
  getNextToken(); // eat identifier

  return std::make_unique<TypeMemberDefinitionExprAST>(
    std::move(name), type_rawptr);
}

// ----------------------------------------------------------------------------

/// tydef ::= tyname ':' 'struct' '{' memdef (',' memdef)* '}'
static std::unique_ptr<ExprAST> ParseCompoundTypeDefinitionExpr()
{
  std::string name = IdentifierStr;

  getNextToken(); // eat the identifier
  if (CurTok != tok_colon)
    return Error("expected ':' after type identifier");

  getNextToken(); // eat the colon
  if (CurTok != tok_struct)
    return Error("expected 'struct' for compound type definition");

  getNextToken(); // eat the 'struct'
  if (CurTok != tok_brace_open)
    return Error("expected opening '{' for compound type definition");

  getNextToken(); // eat the brace

  using MemberDef_t = TypeMemberDefinitionExprAST;
  std::vector<std::unique_ptr<MemberDef_t>> memberDefs;

  while (1)
  {
    auto memberDef = ParseCompoundMemberDefinitionExpr();

    // note that this hack ignores the unique_ptr's deleter, 
    // which is fine as it is defined virtual in the base class
    auto* memberDef_rawptr = static_cast<MemberDef_t*>(memberDef.release());
    memberDefs.push_back(std::unique_ptr<MemberDef_t>(memberDef_rawptr));

    // End of member list, exit loop
    if (CurTok != ',')
      break;

    getNextToken(); // eat the ','

    if (CurTok != tok_identifier)
      return Error("expected identifier for variable type after ','");
  }

  // At this point, we have to have '}'
  if (CurTok != tok_brace_close)
    return Error("expected closing '}' after compound type definition");

  getNextToken(); // eat the brace
  return std::make_unique<TypeDefinitionExprAST>(
    std::move(name), std::move(memberDefs));
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

void TopLevelExprAST::ParseTypeSection()
{
  getNextToken(); // eat the 'types'

  while (1)
  {
    auto typeDef = ParseCompoundTypeDefinitionExpr();

    // note that this hack ignores the unique_ptr's deleter, 
    // which is fine as it is defined virtual in the base class
    auto* typeDef_rawptr = static_cast<TypeDefinitionExprAST*>(typeDef.release());
    TypeDefinitions.push_back(std::unique_ptr<TypeDefinitionExprAST>(typeDef_rawptr));

    if (CurTok != ',')
      break;

    getNextToken(); // eat the ','
    assert(CurTok == tok_identifier && 
          "expected next type identifier after ','");
  }

  assert((CurTok == tok_variables || CurTok == tok_execute) &&
         "expected keywords 'def' or 'run' after 'types'");
}

// ----------------------------------------------------------------------------

void TopLevelExprAST::ParseVarSection()
{
  getNextToken(); // eat the 'def'

  while (1)
  {
    VarDefinitions.push_back(ParseVarDefinitionExpr());

    if (CurTok != ',')
      break;

    getNextToken(); // eat the ','
    assert(CurTok == tok_identifier &&
           "expected next type specifier after ','");
  }

  assert(CurTok == tok_execute && "expected 'run' keyword after 'def'");
}

// ----------------------------------------------------------------------------

void TopLevelExprAST::ParseBody()
{
  getNextToken(); // eat the 'run'
  Body = ParseExpression();
}

// ----------------------------------------------------------------------------

void TopLevelExprAST::InitPrimitiveTypes()
{
  assert(TypeDefinitions.empty());

  auto* tyDouble = new TypeDefinitionExprAST("double", {});
  TypeDefinitions.push_back(std::unique_ptr<TypeDefinitionExprAST>(tyDouble));

  auto* tyInt = new TypeDefinitionExprAST("int", {});
  TypeDefinitions.push_back(std::unique_ptr<TypeDefinitionExprAST>(tyInt));
}

// ----------------------------------------------------------------------------

TypeDefinitionExprAST* TopLevelExprAST::ResolveTypeDefinition(std::string name)
{
  auto matchName = [name](std::unique_ptr<TypeDefinitionExprAST>& ty) {
    return name == ty->getTypeName();
  };

  auto it = std::find_if(TypeDefinitions.begin(), 
                         TypeDefinitions.end(), 
                         matchName);

  return (it == TypeDefinitions.end()) ? nullptr : it->get();
}

// ----------------------------------------------------------------------------

/// toplevelexpr ::= expression
static std::unique_ptr<TopLevelExprAST> ParseTopLevelExpr() 
{
  auto tlExpr = std::make_unique<TopLevelExprAST>();

  // auto resolveTypeFn = std::mem_fn(&TopLevelExprAST::ResolveTypeDefinition);
  // std::bind(resolveTypeFn, tlExpr.get()); // doesn't work with msvc

  resolveTypeHack = [&tlExpr](std::string name) {
    return tlExpr->ResolveTypeDefinition(name);
  };

  tlExpr->InitPrimitiveTypes();

  if (CurTok == tok_types)
  {
    tlExpr->ParseTypeSection();
  }

  if (CurTok == tok_variables)
  {
    tlExpr->ParseVarSection();
  }

  tlExpr->ParseBody();
  return tlExpr;
}
