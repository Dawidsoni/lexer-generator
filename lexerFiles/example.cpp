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

