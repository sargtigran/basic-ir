#ifndef PARSER_H
#define PARSER_H

#include <set>
#include <string>
#include <utility>
#include <vector>

#include "tokens.hxx"
#include "scanner.hxx"
#include "ast.hxx"

/**/
class Parser {
private:
  std::string file;
  Scanner sc;
  Token lookahead;

public:
  Parser(const char* nm) 
    : file{std::string{nm}}, sc{file}
  {}
  Module* parse();

private:
  static std::set<Token> FD;
  static std::set<Token> FS;
  static std::set<Token> FF;
  bool inSet( const std::set<Token>& );

private:
  void match( Token );
  void parseEols();
  pairofstrings parseNameDecl();
  void parseDeclList(vectorofpairsofstrings&);
  void parseType();
  Function* parseDeclare();
  Function* parseSubrHeader();
  Function* parseSubroutine();
  Function* parseFuncHeader();
  Function* parseFunction();
  Statement* parseStatement();
  Statement* parseSequence();
  Statement* parseDim();
  Statement* parseAssignment();
  Statement* parseLet();
  Statement* parseSubCall();
  Statement* parseIf();
  Statement* parseFor();
  Statement* parseWhile();
  Statement* parseInput();
  Statement* parsePrint();
  Statement* parseReturn();
  Expression* parseRelation();
  Expression* parseExpression();
  Expression* parseTerm();
  Expression* parsePower();
  Expression* parseFactor();
};

#endif

