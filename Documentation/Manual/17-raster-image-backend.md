## Raster Image backend

Raster image backend includes a medium that supports:

-   CLIM's medium protocol,

-   CLIM's output stream protocol, and

-   CLIM's Pixmap protocol.

Package `mcclim-raster-image` exports the following macros:

~[Macro]~

mcclim-render:with-output-to-raster-image-stream :
:   *stream-var file-stream format **&rest** options **&body** body*

~[Macro]~

mcclim-render:with-output-to-rgb-pattern :
:   *stream-var image **&rest** options **&body** body*

	Within `body`, `stream-var` is bound to a stream that produces a raster
	image. This stream is suitable as a stream or medium argument to any
	CLIM output utility, such as `draw-line*` or `write-string`.

	The value of `options` is a list consisting of alternating keyword and
	value pairs. These are the supported keywords:

	-   `:width` --- specifies the width of the image. Its default value
		is 1000.
	
	-   `:height` --- specifies the height of the image. Its default value
		is 1000.

~[Macro]~

mcclim-render:with-output-to-raster-image-stream :

:   *stream-var file-stream format **&rest** options **&body** body*

	An image describing the output to the `stream-var` stream will be
	written to the stream `file-stream` using the format `format`.
	`format` is a symbol that names the type of the image. Valid values
	are `:png`, `:jpg`, `:jpeg`, `tiff`, `tif`, `gif`, `pbm`, `pgm`, and
	`ppm`. Its default value is `:png`.

~[Macro]~

mcclim-render:with-output-to-rgb-pattern :

:   *stream-var image **&rest** options **&body** body*

	An image describing the output to the `stream-var` stream will be
	returned as an rgb-pattern (of class `climi::rgb-pattern`).

To run an example:

```commonlisp
(ql:quickload :clim-examples)
(load "Examples/drawing-tests")
(clim-demo::run-drawing-tests)
```
