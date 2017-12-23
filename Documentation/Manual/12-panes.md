## Panes

Panes are subclasses of sheets. Some panes are *layout panes* that
determine the size and position of its children according to rules
specific to each particular type of layout pane. Examples of layout
panes are vertical and horizontal boxes, tables etc.

According to the CLIM specification, all CLIM panes are *rectangular
objects*. For McCLIM, we interpret that phrase to mean that:

-   CLIM panes appear rectangular in the native windowing system;

-   CLIM panes have a native transformation that does not have a
    rotation component, only translation and scaling.

Of course, the specification is unclear here. Panes are subclasses of
sheets, and sheets don't have a shape per-se. Their *regions* may have a
shape, but the sheet itself certainly does not.

The phrase in the specification *could* mean that the *sheet-region* of
a pane is a subclass of the region class *rectangle*. But that would not
exclude the possibility that the region of a pane would be some
non-rectangular shape in the *native coordinate system*. For that to
happen, it would be enough that the *sheet-transformation* of some
ancestor of the pane contain a rotation component. In that case, the
layout protocol would be insufficient in its current version.

McCLIM panes have the following additional restrictions:

-   McCLIM panes have a coordinate system that is only a translation
    compared to that of the frame manager;

-   The parent of a pane is either nil or another pane.

Thus, the panes form a *prefix* in the hierarchy of sheets. It is an
error for a non-pane to adopt a pane.

Notice that the native transformation of a pane need not be the identity
transformation. If the pane is not mirrored, then its native
transformation is probably a translation of that of its parent.

Notice also that the native transformation of a pane need not be the
composition of the identity transformation and a translation. That would
be the case only of the native transformation of the top level sheet is
the identity transformation, but that need not be the case. It is
possible for the frame manager to impose a coordinate system in (say)
millimeters as opposed to pixels. The native transformation of the top
level sheet of such a frame manager is a scaling with coefficients other
than 1.

### Creating panes {#sec-panes-creating}

There is some confusion about the options that are allowed when a pane
is created with `make-pane`. Some parts of the specification suggest
that stream panes such as application panes and interactor panes can be
created using `make-pane` and an option `:scroll-bars`. Since these
application panes do not in themselves contain any scroll bars, using
that option results in a pane hierarchy being created with the topmost
pane being a pane of type `scroller-pane`.

As far as McCLIM is concerned, this option to `make-pane` is obsolete.
^[In the specification, there is no example of the use of this option
to `make-pane` or to the equivalent keywords in the `:panes` section
of `define-application-frame`. There is however one instance where the
`:scroll-bars` option is mention for pane creation. We consider this
to be an error in the specification.] The same goes for using this
option together with the equivalent keyword, i.e., `:application` or
`interactor`, in the `:panes` section of `define-application-frame`.

Instead, we recommend following the examples of the specification, where
scroll bars are added in the `layouts` section of
`define-application-frame`.

When specification talks about panes in a fashion implying some order
(i.e "first application-pane") McCLIM assumes order of definition, not
order of appearing in layout. Particularly that means, that if one pane
is put before another in `:PANES` option, then it precedes it. It is
relevant to `FRAME-STANDARD-OUTPUT` (therefore binding of
`*STANDARD-OUTPUT*`) and other similar functions.

### Pane names

Every pane class accepts the initialization argument `:name` the value
of which is typically a symbol in the package defined by the
application. The generic function `pane-name` returns the value of this
initialization argument. There is no standard way of changing the name
of an existing pane. Using the function `reinitialize-instance` may not
have the desired effect, since the application frame may create a
dictionary mapping names to panes, and there is no way to invalidate the
contents of such a potential dictionary.

The function `find-pane-named` searches the pane hierarchy of the
application frame, consulting the names of each pane until a matching
name is found. The CLIM specification does not say what happens if a
name is given that does not correspond to any pane. McCLIM returns `nil`
in that case. If pane names are not unique, it is unspecified which of
several panes is returned by a call to this function.

If the advice of is followed, then the name given in the `:panes` option
of the macro `define-application-frame` will always be the name of the
top-level pane returned by the *body* following the pane name.

If that advice is not followed, then the name given to a pane in the
`:panes` option of the macro `define-application-frame` may or may not
become the name of the pane that is constructed by the *body* that
follows the name. Recall that the syntax of the expression that defines
a pane in the `:panes` option is `(`*name* `.` *body*`)`. Currently,
McCLIM does the following:

-   If the *body* creates a pane by using a keyword, or by using an
    explicitly mentioned call to `make-pane`, then the name is given to
    the pane of the type explicitly mentioned, even when the option
    `:scroll-bars` is given.

-   If the *body* creates a pane by calling some arbitrary form other
    than a call to `make-pane`, then the name is given to the topmost
    pane returned by the evaluation of that form.

We reserve the right to modify this behavior in the future. Application
code should respect the advice given in .

### Redisplaying panes

Recall that *redisplay* refers to the creation of the output history of
a pane. There are two typical ways of creating this output history:

-   The application maintains some data structure that needs to be
    reflected in the text and graphics of the pane. In this case, a pane
    of type `application-pane` is typically used, and the default value
    of the `:display-time` option is used, which means that some kind of
    application-supplied *display function* is executed at the end of
    each iteration of the command loop. In this situation, the output
    history is either recomputed from scratch in each iteration, or the
    programmer can use the *incremental redisplay* facility to reuse
    some of the existing output records in the history.

-   The application does not keep any data structure, and instead
    generates output incrementally, either as a result of some user
    action, or of some data arriving from an external source. In this
    case, the `:display-time` option is either going to be `t` or `nil`.
    With both of these options, the output history is maintained intact
    after each iteration of the command loop. Instead, when user actions
    are issued, more output records are simply added to the existing
    output history.

For the second possibility, the pane is never redisplayed. Instead, the
action of updating the pane contents is referred to as *replaying* the
output history. The remainder of this section is entirely dedicated to
the *redisplay* action.

It is occasionally necessary for the application to redisplay a pane
explicitly, as opposed to letting the command loop handle it. For
example, if the application data structure is updated in some way, but
this update is not the result of a command, then after such an update,
the redisplay function needs to be executed explicitly. Such an update
could be the result of a timer event, or of communication with an
external process.

~[Generic Function]~

redisplay-frame-pane :

:   *frame pane **&key** force-p*
	
	Calling this generic function causes an immediate redisplay of *pane*.
	When *force-p* is false and the incremental redisplay facility is in use
	for *pane*, then output records are reused as appropriate. Supplying a
	true value for *force-p* causes the entire output history to be
	recomputed from scratch.

	Notice that this function does not check whether the pane has been
	marked to need redisplay, as indicated by a call to the generic function
	`pane-needs-redisplay`. It results in an unconditional redisplay of
	*pane*.

~[Generic Function]~

redisplay-frame-panes :

:   *frame **&key** force-p*

	Calling this generic function causes an immediate redisplay of all the
	panes of *frame* that are visible in the current layout. This function
	simply calls `redisplay-frame-pane` for each visible pane of *frame*.

	Again, notice that no check is being made as to whether the visible
	panes have been marked as needing redisplay. This function calls
	`redisplay-frame-pane` unconditionally for each visible pane, and since
	`redisplay-frame-pane` redisplays the pane unconditionally, it follows
	that all visible panes are unconditionally redisplayed.

Also notice that the implication of this unconditional behavior on the
part of `redisplay-frame-panes` means that this is not the function
called by the standard command loop. The standard command loop only
redisplays panes that have been marked as needing redisplay, though when
the value of the `:display-time` option is `:command-loop` for some
pane, then it is always marked as needing redisplay in each iteration of
the command loop.

### Layout protocol

There is a set of fundamental rules of CLIM dividing responsibility
between a parent pane and a child pane, with respect to the size and
position of the region of the child and the *sheet transformation* of
the child. This set of rules is called the *layout protocol*.

The layout protocol is executed in two phases. The first phase is called
the *space compostion* phase, and the second phase is called the *space
allocation* phase.

#### Space composition

The space composition is accomplished by the generic function
`compose-space`. When applied to a pane, `compose-space` returns an
object of type *space-requirement* indicating the needs of the pane in
terms of preferred size, minimum size and maximum size. The phase starts
when compose-space is applied to the top-level pane of the application
frame. That pane in turn may ask its children for their space
requirements, and so on until the leaves are reached. When the top-level
pane has computed its space requirments, it asks the system for that
much space. A conforming window manager should respect the request
(space wanted, min space, max space) and allocate a top-level window of
an acceptable size. The space given by the system must then be
distributed among the panes in the hierarchy .

Each type of pane is responsible for a different method on
`compose-space`. Leaf panes such as *labelled gadgets* may compute space
requirements based on the size and the text-style of the label. Other
panes such as the vbox layout pane compute the space as a combination of
the space requirements of their children. The result of such a query (in
the form of a space-requirement object) is stored in the pane for later
use, and is only changed as a result of a call to
`note-space-requirement-changed`.

Most *composite panes* can be given explicit values for the values of
`:width`, `:min-width`, `:max-width`, `:height`, `:min-height`, and
`:max-height` options. If such arguments are not given (effectively
making these values nil), a general method is used, such as computing
from children or, for leaf panes with no such reasonable default rule, a
fixed value is given. If such arguments are given, their values are used
instead. Notice that one of `:height` and `:width` might be given,
applying the rule only in one of the dimensions.

Subsequent calls to `compose-space` with the same arguments are assumed
to return the same space-requirement object, unless a call to
note-space-requirement-changed has been called in between.

#### Space allocation

When `allocate-space` is called on a pane `P`, it must compare the
space-requirement of the children of `P` to the available space, in
order to distribute it in the most preferable way. In order to avoid a
second recursive invokation of `compose-space` at this point, we store
the result of the previous call to `compose-space` in each pane.

To handle this situtation and also explicitly given size options, we use
an `:around` method on `compose-space`. The `:around` method will call
the primary method only if necessary (i.e.,
`(eq (slot-value pane ’space-requirement) nil)`), and store the result
of the call to the primary method in the `space-requirement` slot.

We then compute the space requirement of the pane as follows:

```commonlisp
(setf (space-requirement-width ...)  (or explicit-width
	(space-requirement-width request)) ...
	(space-requirement-max-width ...)  (or explicit-max-width
	explicit-width (space-requirement-max-width request)) ...)
```

When the call to the primary method is not necessary we simply return
the stored value.

The `spacer-pane` is an exception to the rule indicated above. The
explicit size you can give for this pane should represent the margin
size. So its primary method should only call compose on the child. And
the around method will compute the explicit sizes for it from the space
requirement of the child and for the values given for the surrounding
space.

#### Change-space Notification Protocol

The purpose of the change-space notification protocol is to force a
recalculation of the space occupied by potentially each pane in the
*pane hierarchy*. The protocol is triggerred by a call to
`note-space-requirement-changed` on a pane `P`. In McCLIM, we must
therefore invalidate the stored space-requirement value and re-invoke
`compose-space` on `P`. Finally, the *parent* of `P` must be notified
recursively.

This process would be repeated for all the panes on a path from `P` to
the top-level pane, if it weren't for the fact that some panes compute
their space requirements independently of those of their children. Thus,
we stop calling `note-space-requirement-changed` in the following cases:

-   when `P` is a `restraining-pane`,

-   when `P` is a `top-level-sheet-pane`, or

-   when `P` has been given explicit values for `:width` and `:height`

In either of those cases, `allocate-space` is called.
