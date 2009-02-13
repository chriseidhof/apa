%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	latexmk -pdf $<
