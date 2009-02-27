chaotic = ghc Chaotic.hs -e
examples = result1y.tex result1r.tex result1a.tex result1empty.tex program1.tex program2.tex 

Exercise1-test.pdf: %.lhs $(examples)
	latexmk -pdf Exercise1.lhs

%.pdf: %.lhs $(examples)
	latexmk -pdf $<

result1y.tex: *.hs
	$(chaotic) "section2 prog1 (Just \"y\")" > $@

result1r.tex: *.hs
	$(chaotic) "section2 prog1 (Just \"r\")" > $@

result1a.tex: *.hs
	$(chaotic) "section2 prog1 (Just \"a\")" > $@

result1empty.tex: *.hs
	$(chaotic) "section2 prog1 Nothing" > $@

result2r.tex: *.hs
	$(chaotic) "section2 prog2 \"r\"" > $@

program1.tex: *.hs
	$(chaotic) "labelProgram prog1" > $@

program2.tex: *.hs
	$(chaotic) "labelProgram prog2" > $@
