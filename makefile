parser:	parser.l parser.y parser.h
	bison -d -t parser.y
	flex parser.l
	gcc -o $@ parser.tab.c lex.yy.c -lfl

clean:	
	rm -rf parser lex.yy.c *.tab.h *.tab.c *.out
