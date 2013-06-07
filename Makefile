
TRANSF_OPTIONS =
PANDOC_OPTIONS = -s --toc -cstyles.css -s -t html --highlight-style=kate

make:
	rm *.png
	( transf $(TRANSF_OPTIONS) | pandoc $(PANDOC_OPTIONS) ) <test.md >test.html
	rm *.ly
	rm *.eps	
	rm *.count
	rm *.tex
	rm *.texi
