# `make pdf' requires pdflatex and builds readme.pdf
# `make txt' requires hevea and builds readme.txt
# `make html' requires hevea and builds readme.html

TEXFILES = atd-annot.tex atdgen-manual.tex atdgen-body.tex

.PHONY: all pdf txt html clean

all: pdf txt html
pdf: atdgen-manual.pdf
txt: atdgen-manual.txt
html: atdgen-manual.html

atdgen-manual.tex: ../ag_version.ml atdgen-manual.mlx
	OCAMLPATH=../..:$$OCAMLPATH \
		camlmix atdgen-manual.mlx -o atdgen-manual.tex

atdgen-body.tex: ../ag_version.ml macros.ml atdgen-body.mlx
	OCAMLPATH=../..:$$OCAMLPATH \
		camlmix atdgen-body.mlx -o atdgen-body.tex

atdgen-manual.txt: $(TEXFILES)
	rm -f *.aux
	hevea -fix -text atdgen-manual
	iconv -f ISO_8859-1 -t UTF-8 < atdgen-manual.txt > ../README

atdgen-manual.html: $(TEXFILES)
	rm -f *.aux
	hevea -fix atdgen-manual
	sed -i '/<\/STYLE>/ r hevea-insert1.html' atdgen-manual.html
	sed -i '/<BODY *>/ r hevea-insert2.html' atdgen-manual.html
	sed -i 's/<\/BLOCKQUOTE><\/BODY>/<\/BLOCKQUOTE><!--END-->\n<\/BODY>/' \
		atdgen-manual.html
	sed -i '/<!--END-->/ r hevea-insert3.html' atdgen-manual.html

atdgen-manual.pdf: $(TEXFILES)
	pdflatex atdgen-manual
	pdflatex atdgen-manual
	pdflatex atdgen-manual

clean:
	rm -f *.aux *.toc *.log *.out *.haux *.htoc *.fls \
		atdgen-body.tex atdgen-manual.tex atdgen-manual.pdf \
		atdgen-manual.txt atdgen-manual.html \
		*.mlx.ml
