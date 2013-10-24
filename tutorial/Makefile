all: tex glossary tex pdf 

glossary: tutorial.tex
	makeglossaries tutorial

pdf: tutorial.tex
	R CMD texi2pdf --build='local' tutorial.tex

tex: tutorial.Rnw
	R CMD Sweave tutorial.Rnw


