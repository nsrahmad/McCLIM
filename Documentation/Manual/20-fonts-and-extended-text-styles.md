## Fonts and Extended Text Styles

### Extended Text Styles

McCLIM extends the legal values for the `family` and `face` arguments to
`make-text-style` to include strings (in additional to the portable
keyword symbols), as permitted by the CLIM spec, section 11.1.

Each backend defines its own specific syntax for these family and face
names.

The CLX backend maps the text style family to the X font's *foundry* and
*family* values, separated by a dash. The face is mapped to *weight* and
*slant* in the same way. For example, the following form creates a text
style for *-misc-fixed-bold-r-\*-\*-18-\*-\*-\*-\*-\*-\*-\**:

```commonlisp
(make-text-style "misc-fixed" "bold-r" 18)
```

In the GTK backend, the text style family and face are used directly as
the Pango font family and face name. Please refer to Pango documentation
for details on the syntax of face names. Example:

```commonlisp
(make-text-style "Bitstream Vera Sans" "Bold Oblique" 54)
```

### Listing Fonts

McCLIM's font listing functions allow applications to list all available
fonts available on a `port` and create text style instances for them.

Example:

```commonlisp
* (find "Bitstream Vera Sans Mono"
		(clim-extensions:port-all-font-families (clim:find-port))
		:key #'clim-extensions:font-family-name
		:test #'equal)
#<CLIM-GTKAIRO::PANGO-FONT-FAMILY Bitstream Vera Sans Mono>

* (clim-extensions:font-family-all-faces *)
(#<CLIM-GTKAIRO::PANGO-FONT-FACE Bitstream Vera Sans Mono, Bold>
#<CLIM-GTKAIRO::PANGO-FONT-FACE Bitstream Vera Sans Mono, Bold Oblique>
#<CLIM-GTKAIRO::PANGO-FONT-FACE Bitstream Vera Sans Mono, Oblique>
#<CLIM-GTKAIRO::PANGO-FONT-FACE Bitstream Vera Sans Mono, Roman>)

* (clim-extensions:font-face-scalable-p (car *))
T
		
* (clim-extensions:font-face-text-style (car **) 50)
#<CLIM:STANDARD-TEXT-STYLE "Bitstream Vera Sans Mono" "Bold" 50>
```

~[Class]~

clim-extensions:font-family :
:   The protocol class for font families. Each backend defines a subclass of
	font-family and implements its accessors. Font family instances are
	never created by user code. Use port-all-font-families to list all
	instances available on a port.

~[Class]~

clim-extensions:font-face :
:   The protocol class for font faces Each backend defines a subclass of
	font-face and implements its accessors. Font face instances are never
	created by user code. Use font-family-all-faces to list all faces of a
	font family.

~[Generic Function]~

clim-extensions:port-all-font-families :
:   *port **&key** invalidate-cache **&allow-other-keys** *

	Returns the list of all `font-family` instances known by `port`. With
	`invalidate-cache`, cached font family information is discarded, if any.

~[Generic Function]~

clim-extensions:font-family-name :
:   *font-family*

	Return the font family's name. This name is meant for user display, and
	does not, at the time of this writing, necessarily the same string used
	as the text style family for this port.

~[Generic Function]~

clim-extensions:font-family-port :
:   *font-family*

	Return the port this font family belongs to.
	
~[Generic Function]~

clim-extensions:font-family-all-faces :
:   *font-family*

	Return the list of all font-face instances for this family.

~[Generic Function]~

clim-extensions:font-face-name :
:   *font-face*

	Return the font face's name. This name is meant for user display, and
	does not, at the time of this writing, necessarily the same string used
	as the text style face for this port.

~[Generic Function]~

clim-extensions:font-face-family :
:   *font-face*

	Return the font family this face belongs to.

~[Generic Function]~

clim-extensions:font-face-all-sizes :
:   *font-face*

	Return the list of all font sizes known to be valid for this font, if
	the font is restricted to particular sizes. For scalable fonts,
	arbitrary sizes will work, and this list represents only a subset of the
	valid sizes. See font-face-scalable-p.

~[Generic Function]~

clim-extensions:font-face-scalable-p :
:   *font-face*

	Return true if this font is scalable, as opposed to a bitmap font. For a
	scalable font, arbitrary font sizes are expected to work.

~[Generic Function]~

clim-extensions:font-face-text-style :
:   *font-face **&optional** size*

	Return an extended text style describing this font face in the specified
	size. If size is nil, the resulting text style does not specify a size.
