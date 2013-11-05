all: tex pdf glossary  

glossary: tutorial.tex
	makeglossaries tutorial
	R CMD texi2pdf --build='local' tutorial.tex

pdf: tutorial.tex
	R CMD texi2pdf --build='local' tutorial.tex

tex: tutorial.Rnw
	R CMD Sweave tutorial.Rnw

	
