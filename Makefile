default: talk.pdf

Pres.tex : Uni.tex Poly.tex

# Pres.% : Uni.% Poly.% --doesn't work

%.pdf : %.tex
	latexmk -pdf $<

%.hs : %.lhs
	lhs2TeX -o $@ --code $<

%.tex : %.lhs
	lhs2TeX -o $@ --tt $<

.PHONY: clean
clean:
	latexmk -CA Pres
