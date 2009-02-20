chaotic = ghc Chaotic.hs -e
## %.tex: %.lhs result1y.tex result1r.tex program1.tex
## 	lhs2Tex -o $@ $<

%.pdf: %.lhs result1y.tex result1r.tex program1.tex program2.tex
	latexmk -pdf $<

result1y.tex: Chaotic.hs
	$(chaotic) "section2 prog1 \"y\"" > $@

result1r.tex: Chaotic.hs
	$(chaotic) "section2 prog1 \"r\"" > $@

result2r.tex: Chaotic.hs
	$(chaotic) "section2 prog2 \"r\"" > $@

program1.tex: Chaotic.hs
	$(chaotic) "labelProgram prog1" > $@

program2.tex: Chaotic.hs
	$(chaotic) "labelProgram prog2" > $@
