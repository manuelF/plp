HDEPS=HUnitLang.lhs HUnitBase.lhs HUnitText.lhs HUnit.lhs
TPDEPS=LPO.hs Tests.hs
DEPS=$(HDEPS) $(TPDEPS)


testTP1:
	ghc -O -H14m -o testTP1 $(DEPS)

%.o: %.lhs
	ghc -O -c $< $@ 

%.o: %.hs
	ghc -O -c $< $@

clean:
	rm -f *.o *.hi testTP1
