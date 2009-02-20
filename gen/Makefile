chaotic = ghc Test.hs -e
examples = result1y.tex result1r.tex result1r_prt.tex result1r_mass.tex result1empty.tex program1.tex 

%.pdf: %.tex $(examples)
	pdflatex -pdf $<

result1y.tex: *.hs
	$(chaotic) "showSLV (S.singleton (V \"y\")) prog" > $@

result1r.tex: *.hs
	$(chaotic) "showSLV (S.singleton (V \"r\")) prog" > $@

result1r_prt.tex: *.hs
	$(chaotic) "showSLV S.empty (progprt \"r\")" > $@

result1r_mass.tex: *.hs
	$(chaotic) "showSLV S.empty (progmass \"r\")" > $@





result1empty.tex: *.hs
	$(chaotic) "showSLV S.empty prog" > $@


result2r.tex: *.hs
	$(chaotic) "section2 prog2 \"r\"" > $@

program1.tex: *.hs
	$(chaotic) "labelProgram prog1" > $@

program2.tex: *.hs
	$(chaotic) "labelProgram prog2" > $@
