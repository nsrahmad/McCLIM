## Output Protocol

### Extensions

~[Generic Function]~

clim-extensions:line-style-effective-thickness :
:   *line-style medium*

	Returns the thickness in device units of a line, rendered on *medium*
	with the style *line-style*.

~[Generic Function]~

clim-extensions:medium-miter-limit :
:   *medium*

	If *line-style-joint-shape* is *:miter* and the angle between two
	consequent lines is less than the values return by
	*clim-extensions:medium-miter-limit*, *:bevel* is used instead.

~[Generic Function]~

(setf output-record-parent) :
:   *parent record*

	Set the parent of the output record *record* to *parent*. The value of
	*parent* may be an output record or `nil`.

~[Generic Function]~

map-over-output-records :
:   *continuation record **&optional** (x-offset 0) (y-offset 0)
    **&rest** continuation-args*

	Maps over all of the children of the *record*, calling *continuation* on
	each one. *function* is a function of one or more arguments.
	*continuation* is also called with all of *continuation-args* as
	\"apply\" arguments.

~[Class]~

standard-output-recording-stream :

:   This class is mixed into some other stream class to add output recording
	facilities. It is not instantiable.

### Clarifications

~[Generic Function]~

replay-output-record :
:   *record stream **&optional** region x-offset y-offset*

	Displays the output captured by *record* on the *stream*, exactly as it
	was originally captured. The current user transformation, line style,
	text style, ink and clipping region of *stream* are all ignored.
	Instead, these are gotten from the output record.

	Only those records that overlap *region* are displayed.
