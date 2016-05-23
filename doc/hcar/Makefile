.PHONY: default clean

default: report.pdf

report.pdf: report.tex hcar.sty androidbreakout.png

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.out *.pdf *.aux *.log *.toc
