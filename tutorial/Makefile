MY_FILE = tutorial

all: tex glossary tex pdf 

glossary: $(MY_FILE).tex
	makeglossaries $(MY_FILE)

pdf: $(MY_FILE).tex
	R CMD texi2pdf $(MY_FILE).tex

tex: $(MY_FILE).Rnw
	R CMD Sweave $(MY_FILE).Rnw


