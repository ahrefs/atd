.PHONY: default all man html clean
default: all
all: man html
man: man1/atdgen.1
html: html/atdgen.html

man1/atdgen.1: atdgen.md
	mkdir -p man1
	pandoc $< -o $@ -t man -s

html/atdgen.html: atdgen.md atdgen.css
	mkdir -p html
	cp atdgen.css html
	pandoc $< -o $@ -t html5 -s --toc -c atdgen.css

clean:
	rm -f *~
	rm -rf man1 html
