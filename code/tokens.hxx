
#ifndef TOKENS_H
#define TOKENS_H

/**/
enum class Token : int {
  xNull,
  xEol,
  xInteger,
  xDouble,
  xTrue,
  xFalse,
  xIdent,
  xDim,
  xAs,
  xType,
  xEnd,
  xDeclare,
  xSubroutine,
  xFunction,
  xReturn,
  xIf,
  xThen,
  xElseIf,
  xElse,
  xFor,
  xTo,
  xStep,
  xWhile,
  xInput,
  xPrint,
  xLPar,
  xRPar,
  xComma,
  xAnd,
  xOr,
  xNot,
  xEq,
  xNe,
  xGt,
  xGe,
  xLt,
  xLe,
  xAdd,
  xSub,
  xMul,
  xDiv,
  xMod,
  xPow,
  xEof
};

  std::string N( Token k );
}

#endif

