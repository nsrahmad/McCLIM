## PostScript backend

### Postscript fonts

Font mapping is a cons, the car of which is the name of the font
(FontName field in the AFM file), and the cdr is the size in points.
Before establishing the mapping, an information about this font should
be loaded with the function `load-afm-file`.

### Additional functions

Package `clim-postscript` exports the following functions:

~[Function]~

load-afm-file : 
:   *afm-filename*

	Loads a description of a font from the specified AFM file.
