MANDIR = ../man1
HTMLDIR = ../html

.PHONY: default all man html clean
default: all
all: man html

man: $(MANDIR)/atdgen.1
html: $(HTMLDIR)/index.html $(HTMLDIR)/atdgen.html $(HTMLDIR)/tutorial.html \
      $(HTMLDIR)/biniou-format.txt

$(MANDIR)/atdgen.1: atdgen.md
	mkdir -p $(MANDIR)
	pandoc $< -o $@ -t man -s

$(HTMLDIR)/atdgen.html: atdgen.md atdgen.css
	mkdir -p $(HTMLDIR)
	cp atdgen.css $(HTMLDIR)
	pandoc $< -o $@ -t html5 -s --toc -c atdgen.css

$(HTMLDIR)/tutorial.html: tutorial.md atdgen.css
	mkdir -p $(HTMLDIR)
	cp atdgen.css $(HTMLDIR)
	pandoc $< -o $@ -t html5 -s --toc -c atdgen.css

$(HTMLDIR)/index.html: index.md atdgen.css
	mkdir -p $(HTMLDIR)
	cp atdgen.css $(HTMLDIR)
	pandoc $< -o $@ -t html5 -s --toc -c atdgen.css

$(HTMLDIR)/biniou-format.txt: biniou-format.txt
	cp $< $@

clean:
	rm -f *~
	rm -rf $(MANDIR) $(HTMLDIR)
