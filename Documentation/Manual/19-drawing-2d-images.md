## Drawing 2D Images
### Images
Images are all rectangular arrangements of pixels. The type of a pixel
depends on the exact type of the image. In addition, a pixel has a color
which also depends on the exact type of the image. You can think of the
color as an interpretation of the pixel value by the type of image.

The coordinate system of an image has (0,0) in its upper-left corner.
The x coordinate grows to the right and the y coordinate downwards.

~[Protocol Class]~

image :
:   This class is the base class for all images.

~[Generic Function]~

image-width :
:   *image*

~[Generic Function]~

image-height :
:   *image*

    These functions return the `width` and `height` of the image
    respectively.

~[Generic Function]~

image-pixels :
:   *image*

    This function returns a two-dimensional array of pixels, whose
    element type depends on the exact subtype of the image.
	
~[Generic Function]~

image-pixels :
:   *image x y*

    This function returns the pixel at the cordinate indicated by the
	value of x and y. The type of the return value depends on the
	exact image type.
	
~[Generic Function]~

(setf image-pixels) :
:   *x y pixel image*
	
	Set the value of the pixel at the coordinate indicated by the values
	of x and y. The exact type acceptable for the pixel argument depends
	on the exact subtype of the image. If x or y are not within the values
	of the width and height of the image, an error is signaled.
	
~[Generic Function]~

image-color :
:   *image x y*

	This function returns the color value of the pixel indicated by the
	values of x and y. The exact type of the return value depends on the
	specific subtype of the image.
	
~[Generic Function]~

(setf image-color) :
:   *x y color image*

	Set the color value of the pixel at the coordinate indicated by the
	values of x and y. The exact type acceptable for the color argument
	depends on the exact subtype of the image. In addition, the exact
	color given to the pixel may be an approximation of the value of the
	color argument. For instance, if the image is a gray-level image, then
	the color given will correspond to the intensity value of the color
	argument. If x or y are not within the values of the width and height
	of the image, an error is signaled.

~[Protocol Class]~

spectral-image :
:   This class is a subclass of the image class. It is the root of a
	subhiearchy for manipulating images represented in various spectral
	formats, other than RGB. \[This subhierarchy will be elaborated later
	in the context of the color model of Strandh and Braquelaire\].
	
~[Protocol Class]~

rgb-image :
:   This class is a subclass of the image class. It is the root of a
	subhierarchy for manipulating images whose pixel colors are
	represented as RGB coordinates. The function image-color always
	returns a value of type (unsigned-byte 24) for images of this type,
	representing three different intensity values of 0-255.
	
~[Protocol Class]~

true-color-image :
:   This class is a subclass of the rgb-image class. Images of this class
	have pixel values of type (unsigned-byte 24). The pixel values
	directly represent RGB values.

~[Protocol Class]~

colormap-image :
:   This class is a subclass of the rgb-image class. Images of this class
	have pixel values that don't directly indicate the color of the pixel.
	The translation between pixel value and color may be implicit (as is
	the case of gray-level images) or explicit with a colormap stored in
	the image object.

~[Protocol Class]~

gray-level-image :
:   This class is a subclass of the colormap-image class. Images of this
	type have pixel values that implicitely represent a gray-level. The
	function pixel-color always returns an RGB value that corresponds to
	the identical intensities for red, green, and blue, according to the
	pixel value.
	
~[Generic Function]~

gray-image-max-levels :
:   *gray-level-image*

	This function returns the maximum number of levels of gray that can be
	represented by the image. The value returned by this function minus
	one would yield a color value of 255,255,255 if it were the value of a
	pixel.
	
~[Generic Function]~

gray-image-max-level :
:   *gray-level-image*

	This function returns the maximum level currently present in the
	image. This function may be very costly to compute, as it might have
	to scan the entire image.
	
~[Generic Function]~

gray-image-min-level :
:   *gray-level-image*

	This function returns the minimum level currently present in the
	image. This function may be very costly to compute, as it might have
	to scan the entire image.
	
~[Class]~

256-gray-level-image :
:   This class is a subclass of the gray-level-image class. Images of this
	type have pixels represented as 8-bit unsigned pixels. The function
	image-pixel always returns a value of type (unsigned-byte 8) for
	images of this type. The function gray-image-max-levels returns 256
	for all instances of this class.
	
~[Class]~

binary-image :
:   This class is a subclass of the gray-level-image class. Images of this
	type have pixel values of type bit. The function image-pixel returns
	values of type bit when applied to an image of this type. The function
	pixel-color returns 0,0,0 for zero-valued bits and 255,255,255 for
	one-valued bits.
	
### Utility Functions
~[Generic Function]~

rotate-image :
:   *image angle **&key** (antialias t)*

~[Generic Function]~

flip-image :
:   *image ...*

~[Generic Function]~

translate-image :
:   *image ...*

~[Generic Function]~

scale-image :
:   *image ...*

### Reading Image Files
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
:   *image destination **&key** (type :pnm) (quality 1)*

	Write the image to the destination. The destination can be a pathname
	designator (a string or a path), or a stream. Valid values of type are
	:pnm (pbm, pgm, or ppm according to the type of image), :png, :jpeg,
	(more\...). The quality argument is a value from 0 to 1 and indicates
	desired image quality (for formats with lossy compression).
