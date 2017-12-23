## Render Images

This extension has the goal to provide a fast and flexible way to
display images in the screen. It is not a general purpose image
processing library (see opticl).

Images are all rectangular arrangements of pixels. The type of a pixel
depends on the exact type of the image. In addition, a pixel has a color
which also depends on the exact type of the image. You can think of the
color as an interpretation of the pixel value by the type of the image.

The coordinate system of an image has (0,0) in its upper-left corner.
The x coordinate grows to the right and the y coordinate downwards.

An image can have an additional alpha channel.

~[Protocol Class]~

image :
:   This class is the base class for all images.

~[Generic Function]~

image-width :
:   *image*
	
~[Generic Function]~

image-height :
:   *image*

	These functions return the width and the height of the image respectively.
	
~[Generic Function]~

image-alpha-p :
:   *image*

	This function returns true if the image has an alpha channel.
	
### Image Mixins
~[Protocol Class]~

rgb-image-mixin :
:   

~[Protocol Class]~

stencil-image-mixin :
:   

### Basic Image
~[Protocol Class]~

basic-image :
:   This class is a subclass of the image class.

~[Generic Function]~

image-pixels :
:   *basic-image*

	This function returns the internal representation of the array of pixels.

### Drawable Images
In order to be drawn in a screen, each pixel must be mapped into its rgb
components. Each component is represented by an octet.

In McCLIM a rgb color is represented as a triple of real number between
0 and 1. Differently, in the image library, when we want drawn an image,
a rgb color must be represented as a triple of octet (integer number
between 0 and 255). In addition, the opacity is represented by an octet
(0 - full transparent, 255 - opaque).

~[Type]~

octet :
: `'(unsigned-byte 8)`

~[Function]~

color-octet->value :
:   *v*

	This function returns `(/ v 255)`.
	
~[Function]~

color-value->octet :
:   *color*

	This function returns `(coerce (round (* 255 color)) 'octet)`.
	
~[Function]~

octen-mult :
:   *o1 o2*

	This function returns a result equal to `(round (* (/ o1 255) (/
    o2 255) 255))`.
	
~[Function]~

octet-blend-function :
:   *r1 g1 b1 o1 r2 g2 b2 o2*

	This function is analogous to the `blend-function` for octet.
	
~[Protocol Class]~

drawable-image :
:   This class is a subclass of the image class. All subclasses must
    implement `map-rgb-color`.
	
By default, in order to draw an image on screen, the backend calls
`map-rgb-color`.

~[Generic Function]~

map-rgb-color :
:   *drawable-image fn*

	This function calls `fn` for each pixels of the image. Function
    `fn` must take `5` arguments namely x, y, red, green and blue.
	
~[Function]~

draw-image :
:   *sheet image **&rest** args **&key** clipping-region
    transformation*
	
	This function draws an image on a sheet.
	
### McCLIM Integration
An image can be used as design or pattern. To draw an image you can also
use `draw-pattern*` or `draw-design`.

~[Class]~

image-design :
:   This class is a subclass of the design class.

~[Function]~

make-image-design :
:   *image*

	This function returns an image-design of the image `image`.
	
~[Class]~

image-pattern :
:   This class is a subclass of the `pattern` and `image-design`
    classes.
	
~[Function]~

make-image-pattern :
:   *image*

	This function returns an `image-pattern` of the image `image`.
	
Every design can be converted into `pixeled-design`.

~[Protocol Class]~

pixeled-design :
:

~[Generic Function]~

pixeled-design-region :
:   *pixeled-design*

~[Generic Function]~

make-pixeled-rgba-octets-fn :
:   *pixeled-design*

~[Generic Function]~

make-pixeled-rgba-octets-unsage-fn :
:   *pixeled-design*

~[Protocol Class]~

pixeled-uniform-design :
:

~[Protocol Class]~

pixeled-functional-design :
:

~[Protocol Class]~

pixeled-image-desing :
:

~[Generic Function]~

make-pixeled-design :
:   *design **&key** foreground background*

### OptiCL Images

~[Protocol Class]~

opticl-image :
:   This class is a subclass of the basic-image and drawable-image
	classes. The generic function image-pixels returns an opticl image.

~[Class]~

opticl-rgb-image :
:   This class is a subclass of the basic-image and opticl-image classes.
	A pixel is a triple of octets that represents the red, green and blue
	component, respectively. A pixel can have an optional octet that
	represents its alpha value.
		
	In the current implementation, `image-pixels` returns an array of type
	`opticl:8-bit-rgba-image`.
	
	
~[Class]~

opticl-gray-level-image :
:   This class is a subclass of the basic-image and opticl-image classes.
	In the current implementation, `image-pixels` returns an array of type
	`8-bit-gray-image`.

~[Class]~

opticl-stencil-image :
:   This class is a subclass of the basic-image and opticl-image classes.
	This image contains only al alpha channel. In the current
	implementation, `image-pixels` returns an array of type
	`8-bit-gray-image`.

### 2D images
~[Protocol Class]~

2d-image :
:   This class is a subclass of the basic-image and drawable-image
	classes. The generic function image-pixels returns a two dimensioanl
	array of pixels.
	
~[Class]~

2d-rgb-image :
:   This class is a subclass of the basic-image and 2d-image classes. A
	pixel is a triple of octets that represents the red, green and blue
	component, respectively. A pixel can have an optional octet that
	represents its alpha value.
		
	`image-pixels` returns a `(simple-array (unsigned-byte 32) (* *))`.
	The pixel values directly represent ABRG octet values.
	
~[Class]~

2d-gray-level-image :
:   This class is a subclass of the basic-image and 2d-image classes.
	`image-pixels` returns a `(simple-array (unsigned-byte 8) (* *))`.

~[Class]~

2d-stencil-image :
:   This class is a subclass of the basic-image and 2d-image classes. This
	image contains only al alpha channel.

	`image-pixels` returns a (simple-array (unsigned-byte 8) (\* \*)).
	
### Operations
~[Generic Function]~

fill-image :
:   *image pixeled-design stencil **&key** x y width height stencil-dx
    stencil-dy*
	
~[Generic Function]~

copy-image :
:   *src-image sx sy width height dst-image x y*

~[Generic Function]~

coerce-image :
:   *image imaeg-class*

### I/O Images

~[Generic Function]~

read-image :
:   *source **&key** type width height*

	Read an image from the source. The source can be a pathname designator
	(a string or a path), or a stream. The caller can supply a value for
	type, width, and height for sources that don't indicate these values.
	A value of nil for type means recognize the type automatically. Other
	values for type are :truecolor (an array of 3-byte color values)
	:256-gray-level (an array of 1-byte gray-level values) :binary (an
	array of bits).
	
~[Generic Function]~

write-image :
:   *image **&key** (type :pnm) (quality 1)*

	Write the image to the destination. The destination can be a pathname
	designator (a string or a path), or a stream. Valid values of type are
	:pnm (pbm, pgm, or ppm according to the type of image), :png, :jpeg,
	(more\...). The quality argument is a value from 0 to 1 and indicates
	desired image quality (for formats with lossy compression).
	
### cl-vectors Integration
~[Generic Function]~

aa-cells-sweep/rectangle :
:   *image pixeled-design state clim-region*

~[Function]~

aa-update-state :
:   *state paths transformation*

~[Function]~

aa-fill-paths :
:   *image pixeled-design paths state transformation clip-region*

~[Function]~

aa-stroke-paths :
:   *image pixeled-design paths line-style state transformation
    clip-region*
	
~[Function]~

make-path :
:   *x y*

~[Function]~

line-to :
:   *path x y*

~[Function]~

close-path :
:   *path*

~[Function]~

stroke-path :
:   *path line-style*
