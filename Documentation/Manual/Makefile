SOURCES=1-Introduction.md 2-demos-and-applications.md \
3-the-first-application.md 4-using-presentation-types.md \
5-using-views.md 6-using-command-tables.md \
7-using-menu-bar.md 8-concepts.md \
9-sheet-hierarchies.md 10-clim-drawing-functions.md \
11-windowing-system-drawing-functions.md 12-panes.md \
13-output-protocol.md 14-command-processing.md \
15-writing-backends.md 16-postscript-backend.md \
17-raster-image-backend.md 18-text-editor-substrate.md \
19-drawing-2d-images.md 20-fonts-and-extended-text-styles.md \
21-tab-layout.md 22-render-images.md 23-debugger.md \
24-inspector.md 25-spec-issues.md 26-glossary.md \
27-development-history.md metadata.yaml

PDF_OPTIONS=--toc -F pandoc-crossref --number-sections \
--template "./template.latex" -V classoption=oneside \
--top-level-division=part --highlight-style=tango

all: pdf html

html: $(SOURCES)
	pandoc -s --toc --mathjax -c template.css --to=html5 $(SOURCES) -o mcclim.html

epub: $(SOURCES)
	pandoc -s -f markdown -t epub3 --toc -c template.css --webtex $(SOURCES) -o mcclim.epub

pdf: $(SOURCES)
	pandoc $(PDF_OPTIONS) $(SOURCES) -o mcclim.pdf

info: $(SOURCES)
	pandoc -F pandoc-crossref -f markdown -t texinfo $(SOURCES) -o mcclim.texinfo
	makeinfo --no-validate --force mcclim.texinfo -o mcclim.info

clean:
	rm -rf mcclim.pdf mcclim.texinfo mcclim.info mcclim.html mcclim.epub
