#pragma once

#include <string>

static int LastChar = ' ';

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,

  // primary
  tok_identifier = -2,
  tok_number = -3,

  // section definition
  tok_types = -4,
  tok_variables = -5,
  tok_execute = -6,

  // type definition atomics
  tok_colon = -7,
  tok_struct = -8,
  tok_brace_open = -9,
  tok_brace_close = -10
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

static std::string testInput;     // code provided by unit test
static int testInputReadPos;

static void SetupTestInput(std::string code) {
  assert(!code.empty() && "Test input string must not be empty");

  testInput = std::move(code);
  testInputReadPos = 0;
  LastChar = ' ';
}

/// advance - get next char from input
static char advance() {
  if (testInput.empty())
    return getchar();

  if (testInputReadPos >= testInput.size())
    return EOF;

  return testInput.at(testInputReadPos++);
}

/// gettok - Return the next token
static int gettok() {
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = advance();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;

    while (isalnum((LastChar = advance())))
      IdentifierStr += LastChar;

    if (IdentifierStr == "run")
      return tok_execute;

    if (IdentifierStr == "def")
      return tok_variables;

    if (IdentifierStr == "types")
      return tok_types;

    if (IdentifierStr == "struct")
      return tok_struct;

    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = advance();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }

  if (LastChar == ':')
  {
    LastChar = advance();
    return tok_colon;
  }

  if (LastChar == '{')
  {
    LastChar = advance();
    return tok_brace_open;
  }

  if (LastChar == '}')
  {
    LastChar = advance();
    return tok_brace_close;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = advance();
  return ThisChar;
}
