TARGET:=Proposal.pdf
SOURCE:=Proposal.pdf $(wildcard pages/*.tex)

LATEX=xelatex

$(TARGET) : $(SOURCE)

%.pdf : %.tex
	$(LATEX) --shell-escape $^
	biber $(^:.tex=)
	$(LATEX) --shell-escape $^
	$(LATEX) --shell-escape $^

.PHONY: clean clean-all

clean:
	rm *.log *.aux *.bcf *.bbl *.toc *.xml *.blg

clean-all: clean
	rm *.pdf
