RSCRIPT = $(R_HOME)/bin/Rscript
 
all: RHRV-quickstar.pdf

clean:
	rm -f index.html
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl
 

RHRV-quickstar.pdf: clean RHRV-quickstar.Rnw
	# Need to generate concordance.tex
	$(RSCRIPT) --vanilla -e "require(highlight); driver <- HighlightWeaveLatex(boxes = TRUE); Sweave( 'RHRV-quickstar.Rnw');"
	$(RSCRIPT) --vanilla -e "require(highlight); driver <- HighlightWeaveLatex(boxes = TRUE); Sweave( 'RHRV-quickstar.Rnw', driver = driver );"
	$(RSCRIPT) -e "tools::texi2dvi( 'RHRV-quickstar.tex', pdf = TRUE, clean = TRUE )"
