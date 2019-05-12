all:
	happy -gca ParReBabel.y
	alex -g LexReBabel.x
	ghc --make TestReBabel.hs -o TestReBabel

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocReBabel.* LexReBabel.* ParReBabel.* LayoutReBabel.* SkelReBabel.* PrintReBabel.* TestReBabel.* AbsReBabel.* TestReBabel ErrM.* SharedString.* ComposOp.* ReBabel.dtd XMLReBabel.* Makefile*
	

