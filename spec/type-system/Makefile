OTT_FILES = grammar.ott rules.ott
# OTT_OPTS = -tex_show_meta false -tex_wrap false -picky_multiple_parses false -tex_suppress_ntr Q
OTT_TEX = type-system.tex

all: type-system.pdf

clean:
	rm -f *.aux *.bbl *.ptb *.pdf *.toc *.out *.run.xml

$(OTT_TEX): $(OTT_FILES)
	ott $(OTT_OPTS) -o $@ $^

%.pdf : %.tex $(OTT_TEX)
	latexmk -pdf $(notdir $*)
