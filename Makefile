all: ast.sml evaluator.sml type-checker.sml calc.lex calc.yacc load-calc.sml loader.mlb
	mllex calc.lex
	ml-yacc calc.yacc
	mlton -output program loader.mlb
clean:
	rm calc.lex.sml
	rm calc.yacc.desc
	rm calc.yacc.sig
	rm calc.yacc.sml
	rm program