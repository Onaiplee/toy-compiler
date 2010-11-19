parser:	parser.l parser.y
				bison -d -t parser.y
				flex parser.l
				gcc -o $@ parser.tab.c lex.yy.c -lfl

clean:	rm lex.yy.c parser.tab.h
