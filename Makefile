default: Pres.pdf

Pres.tex : Uni.tex Poly.tex

%.pdf : %.tex
	latexmk -pdf $<

%.tex : %.lhs
	lhs2TeX -o $@ --tt $<

.PHONY: clean
clean:
	latexmk -CA Pres
