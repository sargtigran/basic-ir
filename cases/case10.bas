
SUB Main
  LET a$ = f$(STR$(7))
END SUB

SUB f$(e$)
  LET f$ = MID$(e$, 2, 1)
END SUB
