
TRANSF_OPTIONS =
PANDOC_OPTIONS = -s --toc -cstyles.css -s -t html --highlight-style=zenburn

make:
	rm -f *.png
	( transf $(TRANSF_OPTIONS) | pandoc $(PANDOC_OPTIONS) ) <test.md >test.html
	rm -f *.ly
	rm -f *.eps	
	rm -f *.count
	rm -f *.tex
	rm -f *.texi
