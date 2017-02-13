#include <iostream>
#include <string>
#include <exception>
using namespace std;

const int STATE_COUNT = 3;//var
const int LETTER_COUNT = 4;//var

enum TokenType {//var
  NUMBER = 0,
  OPERATOR = 1
};

int endStateList[STATE_COUNT] = {-1, NUMBER, OPERATOR};
int letterList[255];
int transList[STATE_COUNT][LETTER_COUNT] = {
  {1, 1, 2, 2},
  {1, 1, -1, -1},
  {-1, -1, -1, -1}
};

class TokenVal {//const
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
  int prevState = 0, curState = 0;
  int textPos = 0;
  while(textPos != text.size()) {
    int letterInd = letterList[text[textPos]];
    if(letterInd == -1) {
      throw TokenException();
    }
    prevState = curState;
    curState = transList[curState][letterInd];
    if(curState == -1 && isEndState(prevState)) {
      return getTokenFuncResult(prevState, text, textPos);
    }else if(curState == -1) {
      throw TokenException();
    }else if(isEndState(prevState) && isEndState(curState) == false) {
      return getTokenFuncResult(prevState, text, textPos);
    }
    textPos++;
  }
  return getTokenFuncResult(curState, text, textPos);
}

int main() {
  for(int i = 0; i < 255; i++) {
    if(i == '1') {
      letterList[i] = 0;
    }else if(i == '2') {
      letterList[i] = 1;
    }else if(i == '+') {
      letterList[i] = 2;
    }else if(i == '*') {
      letterList[i] = 3;
    }else {
      letterList[i] = -1;
    }
  }
  pair<TokenVal, string> tok = getToken("1222*+");
  cout << tok.first.tokenType << " " << tok.first.tokenVal << " " << tok.second << "\n";
}
