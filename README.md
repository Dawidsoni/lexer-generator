# Lexer generator

## Description

This program takes an input file with regular grammar as input and returns a file with C++ function which matches input string with tokens in linear time.
## Sample usage

Input file:

```
tokens :  signedNumber, number, space, operator, lparenthesis, rparenthesis, bin_number, hex_number, float_number

signedNumber => '+' nat | '-' nat
number => nat
nat => '0' nat_aux | '1' nat_aux  | '2' nat_aux | '3' nat_aux | '4' nat_aux
nat => '5' nat_aux | '6' nat_aux  | '7' nat_aux | '8' nat_aux | '9' nat_aux
nat_aux => nat | .

bin_number => bin
bin => '0' bin_aux | '1' bin_aux
bin_aux => bin | 'b'

hex_number => 'F' hex
hex => '0' hex_aux | '1' hex_aux  | '2' hex_aux | '3' hex_aux | '4' hex_aux
hex => '5' hex_aux | '6' hex_aux  | '7' hex_aux | '8' hex_aux | '9' hex_aux
hex => 'A' hex_aux | 'B' hex_aux  | 'C' hex_aux | 'D' hex_aux | 'E' hex_aux | 'F' hex_aux
hex_aux => hex | .

float_number => float
float => '0' float_aux | '1' float_aux  | '2' float_aux | '3' float_aux | '4' float_aux
float => '5' float_aux | '6' float_aux  | '7' float_aux | '8' float_aux | '9' float_aux
float_aux => float | '.' decimal
decimal => '0' decimal_aux | '1' decimal_aux  | '2' decimal_aux | '3' decimal_aux | '4' decimal_aux
decimal => '5' decimal_aux | '6' decimal_aux  | '7' decimal_aux | '8' decimal_aux | '9' decimal_aux
decimal_aux => decimal | .

space => ' ' | ' ' space

operator => '+' | '-' |  '*' |  '/'

lparenthesis => '('

rparenthesis => ')'
```

Output file:

```
#include <string>
#include <exception>
using namespace std;

const int STATE_COUNT = 14;
const int LETTER_COUNT = 25;

enum TokenType {
	SPACE,
	BIN_NUMBER,
	SIGNEDNUMBER,
	FLOAT_NUMBER,
	NUMBER,
	HEX_NUMBER,
	RPARENTHESIS,
	OPERATOR,
	LPARENTHESIS
};

int endStateList[STATE_COUNT] = {-1, -1, SPACE, LPARENTHESIS, NUMBER, NUMBER, RPARENTHESIS, OPERATOR, OPERATOR, HEX_NUMBER, BIN_NUMBER, -1, FLOAT_NUMBER, SIGNEDNUMBER};

int letterList[256] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, -1, -1, -1, 11, 4, 3, 0, -1, 19, 2, 5, 8, 23, 24, 13, 22, 7, 18, 9, 21, 16, -1, -1, -1, -1, -1, -1, -1, 6, 1, 10, 15, 20, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

int transList[STATE_COUNT][LETTER_COUNT] = {
	{8, -1, -1, 7, 6, 7, -1, 5, 4, 5, -1, 3, 2, 5, -1, -1, 5, 1, 5, 8, -1, 5, 5, 4, 5},
	{-1, 9, -1, -1, -1, -1, 9, 9, 9, 9, 9, -1, -1, 9, -1, 9, 9, 9, 9, -1, 9, 9, 9, 9, 9},
	{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{-1, -1, 11, -1, -1, -1, -1, 5, 4, 5, -1, -1, -1, 5, 10, -1, 5, -1, 5, -1, -1, 5, 5, 4, 5},
	{-1, -1, 11, -1, -1, -1, -1, 5, 5, 5, -1, -1, -1, 5, -1, -1, 5, -1, 5, -1, -1, 5, 5, 5, 5},
	{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{-1, -1, -1, -1, -1, -1, -1, 13, 13, 13, -1, -1, -1, 13, -1, -1, 13, -1, 13, -1, -1, 13, 13, 13, 13},
	{-1, 9, -1, -1, -1, -1, 9, 9, 9, 9, 9, -1, -1, 9, -1, 9, 9, 9, 9, -1, 9, 9, 9, 9, 9},
	{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{-1, -1, -1, -1, -1, -1, -1, 12, 12, 12, -1, -1, -1, 12, -1, -1, 12, -1, 12, -1, -1, 12, 12, 12, 12},
	{-1, -1, -1, -1, -1, -1, -1, 12, 12, 12, -1, -1, -1, 12, -1, -1, 12, -1, 12, -1, -1, 12, 12, 12, 12},
	{-1, -1, -1, -1, -1, -1, -1, 13, 13, 13, -1, -1, -1, 13, -1, -1, 13, -1, 13, -1, -1, 13, 13, 13, 13}
};

class TokenVal {
public:
  TokenVal() {}
  TokenVal(TokenType tokenType, string tokenVal) {
    this->tokenType = tokenType;
    this->tokenVal = tokenVal;
  }
  TokenType tokenType;
  string tokenVal;
};

class TokenException : public exception {
   const char* what() const throw() {
      return "Incorrect input string";
   }
};

bool isEndState(int state) {
  return (endStateList[state] != -1);
}

pair<TokenVal, string> getTokenFuncResult(int state, string text, int textPos) {
  TokenVal tokenVal((TokenType)endStateList[state], text.substr(0, textPos));
  string suffix = text.substr(textPos);
  return pair<TokenVal, string>(tokenVal, suffix);
}

pair<TokenVal, string> getToken(string text) {
  int curState = 0, lastEndState = -1, textPos = 0, lastEndPos = -1;
  while(textPos != text.size()) {
    int letterInd = letterList[text[textPos++]];
    if(letterInd == -1) {
			throw TokenException();
		}
    curState = transList[curState][letterInd];
		if(curState != -1 && isEndState(curState)) {
			lastEndState = curState;
			lastEndPos = textPos;
		}
    if(curState == -1 && lastEndState == -1) {
			throw TokenException();
    }else if(curState == -1) {
			return getTokenFuncResult(lastEndState, text, lastEndPos);
    }
  }
	if(lastEndState == -1) {
		throw TokenException();
	}
  return getTokenFuncResult(lastEndState, text, lastEndPos);
}
```
