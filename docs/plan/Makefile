TARGET:=Proposal.pdf
SOURCE:=$(wildcard *.tex) Proposal.bib

LATEX=xelatex

../../$(TARGET) : $(TARGET)
	cp $^ $@

$(TARGET) : $(SOURCE)

%.pdf : %.tex
	$(LATEX) --shell-escape $^
	biber $(^:.tex=)
	$(LATEX) --shell-escape $^
	$(LATEX) --shell-escape $^

.PHONY: clean clean-all

clean:
	rm *.log *.aux *.bcf *.bbl *.toc *.xml *.blg || echo Done

clean-all: clean
	rm $(TARGET) ../../$(TARGET)
