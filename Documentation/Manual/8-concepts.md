# Reference Manual

## Concepts

### Coordinate systems

CLIM uses a number of different coordinate systems and transformations
to transform coordinates between them.

The coordinate system used for the arguments of drawing functions is
called the *user coordinate system*, and coordinate values expressed in
the user coordinate system are known as *user coordinates*.

Each sheet has its own coordinate system called the *sheet coordinate
system*, and positions expressed in this coordinate system are said to
be expressed in *sheet coordinates*. User coordinates are translated to
*sheet coordinates* by means of the *user transformation* also called
the *medium transformation*. This transformation is stored in the
*medium* used for drawing. The medium transformation can be composed
temporarily with a transformation given as an explicit argument to a
drawing function. In that case, the user transformation is temporarily
modified for the duration of the drawing.

Before drawing can occur, coordinates in the sheet coordinate system
must be transformed to *native coordinates*, which are coordinates of
the coordinate system of the native windowing system. The transformation
responsible for computing native coordinates from sheet coordinates is
called the *native transformation*. Notice that each sheet potentially
has its own native coordinate system, so that the native transformation
is specific for each sheet. Another way of putting it is that each sheet
has a mirror, which is a window in the underlying windowing system. If
the sheet has its own mirror, it is the *direct mirror* of the sheet.
Otherwise its mirror is the direct mirror of one of its ancestors. In
any case, the native transformation of the sheet determines how sheet
coordinates are to be translated to the coordinates of that mirror, and
the native coordinate system of the sheet is that of its mirror.

The composition of the user transformation and the native transformation
is called the *device transformation*. It allows drawing functions to
transform coordinates only once before obtaining native coordinates.

Sometimes, it is useful to express coordinates of a sheet in the
coordinate of its parent. The transformation responsible for that is
called the *sheet transformation*.

#### Arguments to drawing functions

Drawing functions are typically called with a sheet as an argument.

A sheet often, but not always, corresponds to a window in the underlying
windowing system.
