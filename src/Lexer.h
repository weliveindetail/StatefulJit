#pragma once

#include <string>

static int LastChar = ' ';

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,

  // primary
  tok_identifier = -5,
  tok_number = -6,

  // section definition
  tok_types = -10,
  tok_variables = -11,
  tok_execute = -12,

  // type definition atomics
  tok_colon = -15,
  tok_struct = -16,
  tok_brace_open = -17,
  tok_brace_close = -18,

  // compound type initialization
  tok_bracket_open = -20,
  tok_bracket_close = -21,

  // separators
  tok_list_separator = -25,
  tok_member_access = -26
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

// advance - get next char from input
static char advance() 
{
  if (testInput.empty())
    return getchar();

  if (testInputReadPos >= testInput.size())
    return EOF;

  return testInput.at(testInputReadPos++);
}

static int matchIdentifierToken(std::string identifier) 
{
  if (identifier == "run")
    return tok_execute;

  if (identifier == "def")
    return tok_variables;

  if (identifier == "types")
    return tok_types;

  if (identifier == "struct")
    return tok_struct;

  return tok_identifier;
}

static int matchSingleCharToken(char character) 
{
  switch (character)
  {
    default: return 0;
    case ':': return tok_colon;
    case '{': return tok_brace_open;
    case '}': return tok_brace_close;
    case '(': return tok_bracket_open;
    case ')': return tok_bracket_close;
    case ',': return tok_list_separator;
    case '.': return tok_member_access;
  }
}

// gettok - Return the next token
static int gettok() 
{
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = advance();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;

    while (isalnum((LastChar = advance())))
      IdentifierStr += LastChar;

    return matchIdentifierToken(IdentifierStr);
  }

  if (isdigit(LastChar)) { // Number: [0-9][0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = advance();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }

  if (int token = matchSingleCharToken(LastChar)) {
    LastChar = advance();
    return token;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = advance();
  return ThisChar;
}
