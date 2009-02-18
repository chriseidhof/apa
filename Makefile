%.tex: %.lhs result1y.tex result1r.tex program1.tex
	lhs2Tex -o $@ $<

%.pdf: %.tex
	latexmk -pdf $<

result1y.tex: Chaotic.hs
	ghc Chaotic.hs -e "section2 \"y\"" > result1y.tex

result1r.tex: Chaotic.hs
	ghc Chaotic.hs -e "section2 \"r\"" > result1r.tex

program1.tex: Chaotic.hs
	ghc Chaotic.hs -e "labelProgram prog1" > program1.tex
