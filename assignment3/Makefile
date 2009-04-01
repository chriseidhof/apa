report.pdf: report.lhs

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	pdflatex $<
