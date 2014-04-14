HDEPS=HUnitLang.lhs HUnitBase.lhs HUnitText.lhs HUnit.lhs
TPDEPS=LPO.hs Tests.hs
DEPS=$(HDEPS) $(TPDEPS)



%.o: %.lhs
	ghc -O -c $< $@ 

%.o: %.hs
	ghc -O -c $< $@

testTP1: $(DEPS)
	ghc -O -H14m -o testTP1 $(DEPS)

clean:
	rm -f *.o *.hi testTP1
