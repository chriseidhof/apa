chaotic = ghc Test.hs -e
programs = prog progprt progmass progbc progbc2
results = resulty.tex resultr.tex resulta.tex resultempty.tex result_lv.tex resultr_prt.tex resultr_mass.tex result_bc.tex result_bc_ae.tex 

%.pdf: %.tex $(programs) $(examples)
	pdflatex $<

prog.tex: *.hs
	$(chaotic) "labelProgram prog" > $@

progprt.tex: *.hs
	$(chaotic) "labelProgram progprt" > $@

progmass.tex: *.hs
	$(chaotic) "labelProgram progmass" > $@

progbc.tex: *.hs
	$(chaotic) "labelProgram progbc" > $@

progbc2.tex: *.hs
	$(chaotic) "labelProgram progbc2" > $@


resulty.tex: *.hs
	$(chaotic) "showSLV (S.singleton (V \"y\")) prog" > $@

resultr.tex: *.hs
	$(chaotic) "showSLV (S.singleton (V \"r\")) prog" > $@

resulta.tex: *.hs
	$(chaotic) "showSLV (S.singleton (V \"a\")) prog" > $@

resultempty.tex: *.hs
	$(chaotic) "showSLV S.empty prog" > $@

result1_lv.tex: *.hs
	$(chaotic) "showLV prog" > $@

resultr_prt.tex: *.hs
	$(chaotic) "showSLV S.empty (progprt \"r\")" > $@

resultr_mass.tex: *.hs
	$(chaotic) "showSLV S.empty (progmass \"r\")" > $@


result_bc.tex: *.hs
	$(chaotic) "showSLV S.empty (progbc \"r\")" > $@

result_bc_ae.tex: *.hs
	$(chaotic) "showAE progbc2" > $@

