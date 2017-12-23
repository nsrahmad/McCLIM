# Auxiliary Materials

## CLIM 2 Specification Issues
### Pane names {#chap-spec-issue-pane-names}

There are several places in the specification where *pane names* are
mentioned.

### Pane initialization and pane properties {#sec-spec-issue-pane-name-pane-initialization-and-pane-properties}

The first place is in sections 29.2.1 (Pane Initialization Options). In
this section, we learn that the keyword argument `:name` can be used as
an initialization option to the function `make-pane` and to the generic
function `make-pane-1`, and that the default value of this option is
`nil`. There is no indication regarding the *type* that the value of
this option can take, but all the examples used in the specification use
symbols for this name. This section also mentions that the `:name`
initialization option must be accepted by all pane classes.

The second place where *pane names* are mentioned is in section 29.2.2
(Pane Properties). In this section, we learn that the generic function
named `pane-name` returns the name of the pane. The is no corresponding
`setf` function, indicating that the name of the pane is immutable.

In other parts of the specification, there are hints that indicate that
panes may be unnamed. Presumably, when `nil` is the value of the
initialization option, this indicates that the pane is unnamed, but this
fact is nowhere explicitly mentioned in the specification.

### Application frame functions

The third place where *pane names* are mentioned is in section 28.3
(Application Frame Functions). There ares several occurrences of pane
names in this section

#### `frame-standard-output`

The description of the generic function `frame-standard-output` says
that the default method returns the first named pane of type
`application-pane` that is visible in the current layout, and that if
there is no such pane, then it returns the first pane of type
`interactor-pane` that is exposed in the current layout.

First of all, the description of this function does not mention what is
returned if there is no pane that fits this description.

More importantly, one may wonder why the candidate application pane has
to be named. As we mentioned in , presumably a pane is considered
unnamed when its name was initialized to `nil`. According to the
specification, an unnamed application pane does not qualify, and if no
named application pane fitting the description can be found, then
instead an interactor pane is chosen. According to the specification,
that interactor pane does not have to be named, however.

#### `frame-standard-input`

The description of the generic function `frame-standard-input` says that
the default method returns the first named pane of type
`interactor-pane` that is visible in the current layout, and that if
there is no such pane, then it returns the value returned by a call to
`frame-standard-output`.

As a consequence of this rule, it appears that it is preferable to have
a named pane than to have a pane of type `interactor-pane`.

#### `frame-panes`

According to the specification, this generic function return the pane
that is the top-level pane in the current layout of the named panes of
the frame given as an argument. It is hard to understand what is meant
by the restriction regarding the names here. Presumably, it is possible
for some children of the top-level pane to be unnamed.

#### `frame-current-panes`

According to the specification, this generic function returns a list of
the named panes in the current layout of the frame given as an argument.
And if there are no named panes, then the specification says that only
the single, top-level pane is returned.

It is not clear whether in the second case, a *list* containing that
single pane is returned, or whether that pane is returned as an atom.

#### `get-frame-pane`

According to the specification, this generic function returns the named
CLIM stream pane with the name given as an argument. Again, no
indication is given as to the nature of the name, nor what equality
function might be used to determine whether the right name has been
found. And, no indication is given as to what happens if there is no
pane with the name given.

#### `find-pane-named`

According to the specification, this generic function returns the pane
with the name given as argument. The difference between this function
and the function `get-frame-pane` is that this function can return a
pane of any type, and not just a CLIM stream pane. Otherwise, the same
omissions apply.

#### `redisplay-frame-pane`

This generic function is called in order to redisplay a particular pane
of a frame. The *pane* argument can be a pane object or the name of a
pane.

### Specifying panes of an application frame

The fourth place where *pane names* are mentioned is in section 28.2.1
(Specifying the Panes of a Frame). The `:panes` option of the
frame-defining macro `define-application-frame` take a list of entries,
where each entry specifies how some pane is to be created. Each entry is
of the very general form `(`*name* `.` *body*`)`. The description of the
syntax of the *body* can be a bit confusing because of the *consing dot*
between the *name* and the *body*, so we will instead describe the forms
that an entire entry can take on.

When the entry has the syntax `(`*name* *compound-form*`)`, then
*compound-form* is simply a form that will be evaluated in order to
return some pane. Presumably, the *name* then becomes the name of the
pane returned by the evaluation of the form. However, since
*compound-form* can consist of some arbitrary code, the only way *name*
can become the name of the resulting pane is to use either some
unspecified way of assigning a (new) name to the page, or to use
`reinitialize-instance`, passing it the `:name` initialization option.
At the time of this writing, McCLIM uses the unspecified way of calling
`(setf slot-value)` using the name of the slot holding the pane name.

If the entry does not have the syntax described in the previous
paragraph, it must have the syntax `(`*name* *pane-type*
*pane-option\...*`)`, where *pane-type* is a symbol that indicates the
type of the pane to be created. For gadgets, the *pane-type* is just the
class name of the abstract gadget type, such as `slider` or
`push-button`. For CLIM stream panes, an abbreviation in the form of a
keyword symbol is possible. Naming the pane in this case is easier,
since it suffices for CLIM to add the pair `:name` *name* to the front
of the list of pane options.

While the description above does not seem to imply any particular
difficulties, things are not as simple as they might seem. The problem
stems from a bunch of small phrases in section 29.4 (CLIM Stream Panes).
Specifically, in section 29.4.2 (CLIM Stream Pane Classes), each
specific pane class mentions default values for pane initialization
options, and in particular for the option `:scroll-bars`. As it happens,
`:scroll-bars` is not one of the possible options mentioned in section
29.4.1 (CLIM Stream Pane Options). Let us analyze a few possible
scenarios:

1.  The `:scroll-bars` option is not a possible option for CLIM stream
    panes. Either it was listed by mistake (not likely), or it was an
    option at some point and was later removed from section 29.4.1 while
    it lingered in the description of the specific panes.

2.  The options listen in section 29.4.2, and specifically the
    `:scroll-bars` option is not an option accepted when an instance of
    a pane class is created, but only as an option supplied to the
    function `make-clim-stream-pane`. In this scenario, when this
    function is called, it does not pass the `:scroll-bars` option on to
    the pane initialization. Instead, it wraps the CLIM stream pane that
    it creates in a `scroller-pane` with the scroll bars indicated, and
    if the option `:scroll-bars` is not given in the call to
    `make-clim-stream-pane`, it is the default in section 29.4.2 that
    applies, depending on the sub-type of the pane to be created.

#### CLIM stream panes do not accept `:scroll-bars`

Scenario number $1$ is likely. In fact, in every example of the use of
the `:panes` option of `define-application-frame` in the CLIM II
specification, there is not a single example of the use of the option
`:scroll-bars`. Instead, scrolling is achieved by the use of the
`scrolling` macro in the `:layouts` option of
`define-application-frame`.

In fact, since scrolling is always achieved by the use of a scroller
pane, if it were possible to supply a `:scroll-bars` option in the
`:panes` option of `define-application-frame` for the creation of a CLIM
stream pane, then the resulting pane would not be a CLIM stream pane,
but some layout pane containing a scroller pane, scroll-bar panes, and
the application pane. And then, to which of these panes would the *name*
be attached?

Even though the specification does not contain any example of using the
`:scroll-bars` option in the `:panes` option of
`define-application-frame`, unfortunately the Genera CLIM user manual
does contain such an example (for the 15-puzzle). The existence of this
example is unfortunate, because without it, scenario is not only very
likely; we could declare it so and eliminate the `:scroll-bars` option
from the CLIM stream pane classes.

#### The `:scroll-bars` option is for `make-clim-stream-pane`

Scenario number $2$ is unlikely. The reason is that the function
`make-clim-stream-pane` lists its own defaults for the `:scroll-bars`
option. If scenario number $2$ were correct, the description of this
function would instead say that the default for the option depends on
the type of the pane to be created.

### Dilemma to resolve

Currently, McCLIM stream panes accepts the `:scroll-bars` initialization
option. For example, for the `application-pane` the default is `t` as
indicated in section 29.4.2. As a consequence, if the application
programmer does not supply the `:scroll-bars` option in the definition
of the application frame, nor any scroller pane, then the resulting
application pane will still have scroll bars.

Furthermore, if the application programmer follows the examples in the
CLIM specification, supplying a scroller pane in the `:layouts` option
of the macro `define-application-frame`, then the application pane will
have a double set of scroll bars.

In order to avoid breaking existing applications, we must suggest a
migration path. One possibility is to recommend that users temporarily
supply the option `:scroll-bars nil` to the pane specification in
`define-application-frame`, and instead indicate scrolling behavior in
the `:layouts` option of the macro `define-application-frame`. At some
later date, we can remove the current default behavior with respect to
scroll bars for the application pane, while still accepting the
`:scroll-bars` option. Finally, at some even later date, we can remove
that option entirely.
