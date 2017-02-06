RPROG=  $(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript
 
all: RHRV-quickstart.pdf

clean:
	rm -f index.html
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl
 

RHRV-quickstart.pdf: clean RHRV-quickstart.Rnw
	$(RSCRIPT) -e "require( 'highlight'); require('tools'); Sweave('RHRV-quickstart.Rnw', driver=HighlightWeaveLatex()); texi2dvi('RHRV-quickstart.tex', pdf=TRUE, clean=TRUE)"
	rm -f RHRV-quickstart.tex  Rplots.pdf
