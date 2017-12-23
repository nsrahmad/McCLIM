## Glossary

Direct mirror :
:   A *mirror* of a sheet which is not shared with any of the ancestors of
	the sheet. All grafted McCLIM sheets have mirrors, but not all have
	direct mirrors. A McCLIM sheet that does not have a direct mirror uses
	the direct mirror of its first ancestor having a direct mirror for
	graphics output. Asking for the direct mirror of a sheet that does not
	have a direct mirror returns nil.

	Whether a McCLIM sheet has a direct mirror or not, is decided by the
	frame manager. Some frame managers may only allow for the graft to be a
	mirrored sheet. Even frame managers that *allow* hierarchical mirrors
	may decide not to allocate a direct mirror for a particular sheet.
	Although sheets with a direct mirror must be instances of the class
	mirrored-sheet-mixin, whether a McCLIM sheet has a direct mirror or not
	is not determined statically by the class of a sheet, but dynamically by
	the frame manager.

Mirror :
:   A device window such as an X11 window that parallels a *sheet* in the
	CLIM *sheet hierarchy*. A *sheet* having such a *direct* mirror is
	called a *mirrored sheet*. When *drawing functions* are called on a
	*mirrored sheet*, they are forwarded to the host windowing system as
	drawing commands on the *mirror*.
	
	CLIM *sheet*s that are not mirrored must be *descendents* (direct or
	indirect) of a *mirrored sheet*, which will then be the *sheet* that
	receives the drawing commands.
	
Mirrored sheet :
:   A *sheet* in the CLIM *sheet hiearchy* that has a direct parallel
	(called the *direct mirror*) in the host windowing system. A mirrored
	sheet is always an instance of the class `mirrored-sheet-mixin`, but
	instances of that class are not necessarily mirrored sheets. The sheet
	is called a mirrored sheet only if it currently has a direct mirror.
	There may be several reasons for an instance of that class not to
	currently have a direct mirror. One is that the sheet is not *grafted*.
	Only grafted sheets can have mirrors. Another one is that the *frame
	manager* responsible for the look and feel of the sheet hierarchy may
	decide that it is inappropriate for the sheet to have a direct mirror,
	for instance if the underlying windowing system does not allow nested
	windows inside an application, or that it would simply be a better use
	of resources not to create a direct mirror for the sheet. An example of
	the last example would be a stream pane inside a the *viewport* of a
	*scroller pane*. The graphics objects (usually text) that appear in a
	stream pane can have very large coordinate values, simply because there
	are many lines of text. Should the stream pane be mirrored, the
	coordinate values used on the mirror may easily go beyond what the
	underlying windowing system accepts. X11, for instance, can not handle
	coordinates greater than 64k (16 bit unsigned integer). By not having a
	direct mirror for the stream pane, the coordinates will be translated to
	those of the (not necessarily direct) mirror of the *viewport* before
	being submitted to the windowing system, which gives more reasonable
	coordinate values.

	It is important to realize the implications of this terminology. A
	mirrored sheet is therefore not a sheet that has a mirror. All grafted
	sheets have mirrors. For the sheet to be a mirrored sheet it has to have
	a *direct* mirror. Also, a call to  returns a mirror for
	all grafted sheets, whether the sheet is a mirrored sheet or not. A call
	to , on the other hand, returns nil if the sheet
	is not a mirrored sheet.

Mirror transformation :
:   The transformation that transforms coordinates in the coordinate system
	of a mirror (i.e. the native coordinates of the mirror) to native
	coordinates of its parent in the underlying windowing system. On most
	systems, including X, this transformation will be a simple translation.

Native coordinates :
:   Each mirror has a coordinate system called the native coordinate system.
	Usually, the native coordinate system of a mirror has its origin in the
	upper-left corner of the mirror, the x-axis grows to the right and the
	y-axis downwards. The unit is usually pixels, but the frame manager can
	impose a native coordinate system with other units, such as millimeters.
	
	The native coordinate system of a sheet is the native coordinate system
	of its mirror (direct or not). Thus, a sheet without a direct mirror has
	the same native coordinate system as its parent. To obtain native
	coordinates of the parent of a mirror, use the *mirror transformation*.

Native region :
:   The native region of a sheet is the intersection of its region and the
	sheet region of all of its parents, expressed in the *native
	coordinates* of the sheet.

Potentially visible area :
:   A bounded area of an otherwise infinte drawing plane that is visible
	unless it is covered by other visible areas.

Sheet coordinates :
:   The coordinate system of coordinates obtained by application of the
	*user transformation*.

Sheet region :
:   The *region* of a sheet determines the visible part of the drawing
	plane. The dimensions of the sheet region are given in *sheet
	coordinates*. The location of the visible part of a sheet within its
	*parent sheet* is determined by a combination of the *sheet
	transformation* and the position of the sheet region.
	
	For instance, assuming that the sheet region is a rectangle with its
	upper-left corner at (2, 1) and that the sheet transformation is a
	simple translation (3, 2). Then the origin of the *sheet coordinate
	system* is at the point (3, 2) within the *sheet coordinate system* of
	its *parent sheet*. The origin of its the coordinate system is not
	visible, however, because the visible region has its upper-left corner
	at (2, 1) in the *sheet coordinate system*. Thus, the visible part will
	be a rectangle whose upper-left corner is at (5, 3) in the *sheet
	coordinate system* of the *parent sheet*.

	Panes and gadgets alter the region and *sheet transformation* of the
	underlying sheets (panes and gadgets are special kinds of sheets) to
	obtain effects such as scrolling, zooming, coordinate system
	transformations, etc.

Sheet transformation :
:   The transformation used to transform *sheet coordinates* of a sheet to
	*sheet coordinates* of its *parent sheet*. The sheet transformation
	determine the position, shape, etc. of a sheet within the coordinate
	system of its parent.

	Panes and gadgets alter the transformation and *sheet region* of the
	underlying sheets (panes and gadgets are special kinds of sheets) to
	obtain effects such as scrolling, zooming, coordinate system
	transformations, etc.

User Clipping region :
:   A *clipping region* used to limit the effect of *drawing functions*. The
	user *clipping region* is stored in the *medium*. It can be altered
	either by updating the *medium*, or by passing a value for the
	:clipping-region *drawing option* to a *drawing function*.

User Coordinates :
:   The coordinate system of coordinates passed to the *drawing functions*.

User Transformation :
:   A transformation used to transform *user coordinates* into *sheet
	coordinates*. The user transformation is stored in the *medium*. It can
	be altered either by updating the *medium*, or by passing a value for
	the :transformation *drawing option* to a *drawing function*.

Visible area :
:   
