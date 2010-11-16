parser:	parser.l parser.y
				bison -d parser.y
				flex parser.l
				cc -o $@ parser.tab.c lex.yy.c -lfl
