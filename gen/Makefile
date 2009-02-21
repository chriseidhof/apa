chaotic = ghc Test.hs -e
programs = prog.tex progprtr.tex progmassr.tex progbc.tex progbc2.tex
results = resulty.tex resultr.tex resulta.tex resultempty.tex result_lv.tex resultr_prt.tex resultr_mass.tex result_bc.tex result_bc_ae.tex 

%.pdf: %.tex $(programs) $(results)
	pdflatex $<

prog.tex: *.hs
	$(chaotic) "labelProgram prog" > $@

progprtr.tex: *.hs
	$(chaotic) "labelProgram (progprt \"r\")" > $@

progmassr.tex: *.hs
	$(chaotic) "labelProgram (progmass \"r\")" > $@

progbc.tex: *.hs
	$(chaotic) "labelProgram (progbc \"r\")" > $@

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

result_lv.tex: *.hs
	$(chaotic) "showLV prog" > $@

resultr_prt.tex: *.hs
	$(chaotic) "showSLV S.empty (progprt \"r\")" > $@

resultr_mass.tex: *.hs
	$(chaotic) "showSLV S.empty (progmass \"r\")" > $@


result_bc.tex: *.hs
	$(chaotic) "showSLV S.empty (progbc \"r\")" > $@

result_bc_ae.tex: *.hs
	$(chaotic) "showAE progbc2" > $@
