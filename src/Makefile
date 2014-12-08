MANDIR = ../man1
HTMLDIR = ../html

.PHONY: default all man html clean
default: all
all: man $(HTMLDIR)

man: $(MANDIR)/atdgen.1
$(HTMLDIR): $(HTMLDIR)/atdgen.html

$(MANDIR)/atdgen.1: atdgen.md
	mkdir -p $(MANDIR)
	pandoc $< -o $@ -t man -s

$(HTMLDIR)/atdgen.html: atdgen.md atdgen.css
	mkdir -p $(HTMLDIR)
	cp atdgen.css $(HTMLDIR)
	pandoc $< -o $@ -t html5 -s --toc -c atdgen.css

clean:
	rm -f *~
	rm -rf $(MANDIR) $(HTMLDIR)
