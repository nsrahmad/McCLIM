# McCLIM native TrueType implementation internal notes

The mcclim-fonts/truetype system extends the CLX backend with
antialiased font rendering in 100% Common Lisp (no foreign code),
using the `XRender` extension and the libraries `zpb-ttf` and
`cl-vectors`.

## mcclim-native-ttf

This component contains native implementation of ttf fonts access and
drawing. It is decoupled from `xrender-fonts` component and may be eventually
reused in the future.

Also implementation of `Font listing extension` (see
`Core/clim-basic/ports.lisp`) is provided (except port part, because
it is dependent on the backend which uses `mcclim-native-ttf`).

### Features

* Kerning
* Tracking (letter-spacing)
* Leading (line-spacing)
* Boxes for missing glyphs
* Transformations

### TODO

* Implement fixed-font-width-p for zpb-ttf.
* Make certain left/right bearings and text-bounding-rectangle are
  correct. text-bounding-rectangle and text-size are quite incorrect especially
  when we take multiline into account and align-x/align-y to center/bottom (not
  to mention toward-x/toward-y which are not implemented at all).
* Rethink interface and make it play well with text-style protocol.

### Wish-list

* Subpixel antialiasing. It would be straightforward to generate the
  glyphs by tripling the width as passed to cl-vectors and compressing
  triplets of pixels together ourselves. I'm not certain how to draw
  the result through xrender. I've seen hints on Google that there is
  subpixel AA support in xrender, which isn't obvious from CLX or the 
  spec. Failing that, we could use a 24bpp mask with component-alpha. 
  That might even be how you're supposed to do it. I'm skeptical as to 
  whether this would be accelerated for most people.

* Subpixel positioning. Not hard in principle - render multiple versions
  of each glyph, offset by fractions of a pixel. Horizontal positioning
  is more important than vertical, so 1/4 pixel horizontal resolution
  and 1 pixel vertical resolution should suffice. Given how ugly most
  CLIM apps are, and the lack of WYSIWYG document editors crying out 
  for perfect text spacing in small fonts, we don't really need this.

## xrender-fonts

Component is responsible for glueing `mcclim-native-ttf` and the
XRender extension in our CLX backend. Moreover it provides port-wise
part of the implementation for `Font listing extension`
(`port-all-font-families` and `register-all-ttf-fonts`).

We support both `standard-text-style` and `device-font-text-style`. The former
verifies in `text-style-to-font` if the `text-style` is already registered in
the system or if we can load it from the provided mappings.
