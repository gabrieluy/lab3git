all:
	happy -gca ParPascal.y
	alex -g LexPascal.x
	ghc --make TestPascal.hs -o TestPascal

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocPascal.* LexPascal.* ParPascal.* LayoutPascal.* SkelPascal.* PrintPascal.* TestPascal.* AbsPascal.* TestPascal ErrM.* SharedString.* ComposOp.* Pascal.dtd XMLPascal.* Makefile*
	

