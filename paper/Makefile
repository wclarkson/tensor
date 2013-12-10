paper: paper.tex references.bib
	pdflatex -halt-on-error paper.tex
	bibtex paper.aux
	pdflatex -halt-on-error paper.tex

clean:
	rm -f *.pdf *.aux *.log *.out *.bbl *.blg
