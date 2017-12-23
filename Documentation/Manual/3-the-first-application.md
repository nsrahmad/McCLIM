## The first application

### A bit of terminology

CLIM was developed before the GUI toolkits widely used at the moment.
Qt, GTK and others appeared much later than CLIM and the difference of
terminology reflects this.

A CLIM application is made up of a hierarchy of an , and (gadgets are
special kinds of panes):

-   An *application frame* is what would usually be called an
    application.

-   At a very high level, panes describe an application frame's visual
    building blocks: a side bar, a menu bar, a table displaying a list
    of items, a text input are all panes. They can be used by
    application programmers to compose the top-level user interface of
    their applications, as well as auxiliary components such as menus
    and dialogs. In addition, panes can be more abstract such as layout
    panes such as , to arrange other panes horizontally or vertically,
    etc.

-   *gadgets* correspond to what other toolkits call *widgets* and
    *control*. Frequently used CLIM gadgets are s, s, etc.

### How CLIM applications produce output

Although it is easy to imagine panes in term of their appearance on
screen, they are much richer: they are actually the series of operations
that produces that appearance. They are not only the end product visible
on a screen, but they contain all the step-by-step information that led
to that representation.

More precisely, CLIM panes record the series of operations that produces
that generates an output. This means that such a pane maintains a
display list, consisting of a sequence of output records, ordered
chronologically, from the first output record to be drawn to the last.

This display list is used to fill in damaged areas of the pane, for
instance as a result of the pane being partially or totally covered by
other panes, and then having some or all of its area again becoming
visible. The output records of the display list that have some parts in
common with the exposed area are partially or totally replayed (in
chronological order) to redraw the contents of the area.

An application can have a pane establish this display list in several
fundamentally different ways, each more sophisticated:

Simple application:

:   Very simple applications have no internal data structure to keep
    track of application objects, and simply produce output to the pane
    from time to time as a result of running commands, occasionally
    perhaps erasing the pane and starting over. Such applications
    typically use text or graphics output as a result of running
    commands. CLIM maintains the display list for the pane, and adds
    to the end of it, each time also producing the rendering
    instructions ^[rendering is carried out by a backend. X11 is
    currently the standard backend.] that result from drawing the new
    output record. If the pane uses scrolling (which it typically
    does), then CLIM must determine the extent of the pane so as to
    update the scroll bar after each new output.

Application with a static display function:

:   More complicated applications use a display function. Before the
    display function is run, the existing display list is typically
    deleted, so that the purpose of the display function becomes to
    establish an entirely new display list. The display function might
    for instance produce some kind of form to be filled in, and
    application commands can use text or graphics operations to fill in
    the form. A game of tic-tac-toe could work this way, where the
    display function draws the board and commands draw shapes into the
    squares.

Application with a dynamic display function:

:   Even more complicated applications might have some internal data
    structure that has a direct mapping to output, and commands simply
    modify this internal data structure. In this case, the display
    function is run after each time around the command loop, because a
    command can have modified the internal data structure in some
    arbitrary ways. (Commands are naturally commands issued by the user
    of the application, but can also be internal commands: a web page
    can be refreshed by a web browser user or by the web page triggering
    an auto-refresh.) Some such applications might simply want to delete
    the existing display list and produce a new one each time (to
    minimize flicker, double buffering could be used). This is a very
    simple way of structuring an application, and entirely acceptable in
    many cases. Consider, for instance, a board game where pieces can be
    moved (as opposed to just added). A very simple way of structuring
    such an application is to have an internal representation of the
    board, and to make the display function traverse this data structure
    and produce the complete output each time in the command loop.

Application with an incremental static display function:

:   Some applications have very large internal data structures to be
    displayed, and it would cause a serious performance problem if the
    display list had to be computer from scratch each time around the
    command loop. To solve this problem, CLIM contains a feature called
    incremental redisplay. It allows many of the output records to be
    kept from one iteration of the command loop to the next. This can be
    done in two different ways. The simplest way is for the application
    to keep the simple structure which consists of traversing the entire
    data structure each time, but at various points indicate to CLIM
    that the output has not changed since last time, so as to avoid
    actually invoking the application code for computing it. This is
    accomplished by the use of `updating-output`. The advantage of
    `updating-output` is that the application logic remains
    straightforward, and it is up to CLIM to do the hard work of
    recycling output records. The disadvantage is that for some very
    demanding applications, this method might not be fast enough.

Programmer does it all:

:   The other way is more complicated and requires the programmer to
    structure the application differently. Essentially, the application
    has to keep track of the output records in the display list, and
    inform CLIM about modifications to it. The main disadvantage of this
    method is that the programmer must now write the application to keep
    track of the output records itself, as opposed to leaving it to
    CLIM.

In the next sections, we will give examples of such examples with
increasing levels of complexity.

### Defining Application Frames

Each CLIM application is defined by an . An application frame is an
instance of the class . As a CLIM user, you typically define a class
that inherits from the class , and that contains additional slots needed
by your application. It is considered good style to keep all your
application-specific data in slots in the application frame (rather
than, say, in global variables), and to define your application-specific
application frame in its own package.

The usual way to define an application frame is to use the macro . This
macro works much like , but also allows you to specify the hierarchy of
and to use.

### A First Attempt

Let us define a very primitive CLIM application. For that, let us put
the following code in a file:

```commonlisp
(in-package :common-lisp-user)

(defpackage :my-first-app
  ;; Imports the appropriate CLIM library
  (:use :clim :clim-lisp)

  ;; The package will only export a function to run the app
  (:export run-my-first-app))

;; Good practice
(in-package :my-first-app)

;; Definition of the structure of a minimum app
(define-application-frame my-first-clim-app ()
  ()

  ;; This app only has 1 pane
  (:panes
   (my-interactor :interactor
                  :height 400
                  :width 600))

  ;; :layouts section describes how the pane is positioned inside
  ;; the application frame.
  ;; With 1 pane, no point getting complicated, Default is fine...
  (:layouts
    (my-default my-interactor)))

;; Now that the structure of the app is defined, need a function
;; to launch an instance of this app. (The user could run
;; several instances of the same app.)
(defun run-my-first-app ()
  (run-frame-top-level (make-application-frame 'my-first-clim-app)))
```

As we can see in this example, we have put our application in a separate
package, here a package named `my-first-app`. While not required,
putting the application in its own package is good practice.

The package for the application uses two packages: and . The package is
the one that contains all the symbols needed for using CLIM. The package
replaces the package for CLIM applications. It is essentially the same
as the package as far as the user is concerned.

In our example, we export the symbol that corresponds to the main
function to start our application, here called `run-my-first-app`.

The most important part of the code in our example is the definition of
the application-frame. In our example, we have defined an application
frame called `my-first-clim-app`, which becomes a CLOS class that
automatically inherits from some standard CLIM application frame class.

The second argument to is a list of additional superclasses from which
you want your application frame to inherit. In our example, this list is
empty, which means that our application frame only inherits from the
standard CLIM application frame.

The third argument to is a list of CLOS slots to be added to any
instance of this kind of application frame. These slots are typically
used for holding all application-specific data. The current instance of
the application frame will always be the value of the special variable
`*application-frame*`, so that the values of these slots can be
accessed. In our example, we do not initially have any further slots.

The rest of the definition of an application frame contains additional
elements that CLIM will allow the user to define. In our example, we
have two additional (mandatory) elements: `:panes` and `:layouts`.

The `:panes` element defines a collection of CLIM panes that each
instance of your application may have. Each pane has a name, a type, and
perhaps some options that are used to instantiate that particular type
of pane. Here, we have a pane called `my-interactor` of type
`:interactor` with a height of 400 units and a width of 600 units. In
McCLIM, the units are initially physical units (number of pixels) of the
native windowing system. An typical application would have many
different `panes`: a word processor would have a `pane` for the text, a
`pane` for the style sheet, a `pane` for reviewing text changes, and so
on. The fact that a `pane` is defined does not mean that they they will
all be visible at all times. This section merely defines them.

The `:layouts` element defines one or more ways of organizing the panes
in a hierarchy. Each layout has a name and a description of a hierarchy.
In our example, only one layout, named `my-default`, is defined. The
layout called `default` is the one that is used by CLIM at startup. In
our example, the corresponding hierarchy is trivial, since it contains
only the one element `int`, which is the name of our only pane.

### Executing the Application

In order to run a CLIM application, you must have a Lisp system that
contains McCLIM. If you use CMUCL or SBCL, you either need a `core` file
that already has McCLIM in it, or else, you have to load the McCLIM
compiled files that make up the McCLIM distribution. The fist solution
is recommended so as to avoid having to load the McCLIM files each time
you start your CLIM application.

To execute the application, load the file containing your code (possibly
after compiling it) into your running Lisp system. Then start the
application. Our example can be started by typing `(app:app-main)`.

### Adding Functionality

In a serious application, you would probably want some area where your
application objects are to be displayed. In CLIM, such an area is called
an *application pane*, and would be an instance (direct or indirect) of
the CLIM class `application-pane`. In fact, as mentioned earlier,
`panes` and by extension `application-panes` are better understood as
the list of instructions that yield a particular visual representation.
More precisely, `application-pane` instances are in reality also
*streams* which can be used in calls both to ordinary input and output
functions such as `format` and `read` and to CLIM-specific functions
such as `draw-line`.

Let's consider an improved example, where for sake of efficiency the
*my-* names have been replaced by shorter versions:

```commonlisp
(in-package :common-lisp-user)

(defpackage :app
  (:use :clim :clim-lisp)
  (:export run-app))

(in-package :app)

(define-application-frame superapp ()
  ()
  (:pointer-documentation t)
  (:panes

   ;; Let's add an additional pane
   (app :application

        ;; When should this pane be displayed in the command loop.
        ;; Note that the refresh is pane-specific, not
        ;; application-wide.
        :display-time nil
        :height 400
        :width 600)

   (int :interactor
        :height 200
        :width 600))

  (:layouts

   ;; This time we explicitly specify that the 2 defined panes
   ;; should be stacked vertically.
   (default (vertically ()
              app int))))

;;
;; Let's also define commands that will act on the application.
;;

;; How to leave the application.
;; Note the '-superapp-' part of the command definition, coming from
;; the name of the application frame.
(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))


;; This is an additional command that will be used in the next
;; example, so it's content is not important. However, it is useful
;; to describe some aspect of the command loop. See below.
(define-superapp-command (com-parity :name t) ((number 'integer))
  (format t "~a is ~a~%" number
          (if (oddp number)
              "odd"
              "even")))
			  
(defun run-app ()
  (run-frame-top-level (make-application-frame 'superapp)))
```
In this example, we have such an application pane, the name of which is
`app`. As you can see, we have defined it with an option
`:display-time nil`. The default value for this option for an
application pane is `:command-loop`, which means that the pane is
cleared after each iteration in the command loop, and then redisplayed
using a client-supplied *display function*. The default display function
does nothing, and we have not supplied any, so if we had omitted the
`:display-time nil` option, the `parity` command would have written to
the pane. Then, at the end of the command loop, the pane would have been
cleared, and nothing else would have been displayed. The net result is
that we would have seen no visible output. With the option
`:display-time nil`, the pane is never cleared, and output is
accumulated every time we execute the `parity` command.

For this example, let us also add a few *commands*. Such commands are
defined by the use of a macro called , where *name* is the name of the
application, in our case `superapp`. This macro is automatically defined
by `define-application-frame`.

Let us also add a pane that automatically provides documentation for
different actions on the pointer device.

If you execute this example, you will find that you now have three
different panes, the application pane, the interactor pane and the
pointer documentation pane. In the pointer documentation pane, you will
see the text `R possibilities` which indicates that if you click the
right mouse button, you will automatically see a popup menu that lets
you choose a command. In our case, you will have the default commands
that are automatically proposed by McCLIM plus the commands that you
defined yourself, in this case `quit` and `parity`.

Notice that commands, in order to be available from the command line,
must have an option of `:name t`. The reason is that some commands will
be available only from menus or by some other mechanism.

You may notice that if the output of the application is hidden (say by
the window of some other application) and then re-exposed, the output
reappears normally, without any intervention necessary on the part of
the programmer. This effect is accomplished by a CLIM mechanism called
*output recording*. Essentially, every piece of output is not only
displayed in the pane, but also captured in an *output record*
associated with the pane. When a pane is re-exposed, its output records
are consulted and if any of them overlap the re-exposed region, they are
redisplayed. In fact, some others may be redisplayed as well, because
CLIM guarantees that the effect will be the same as when the initial
output was created. It does that by making sure that the order between
(partially) overlapping output records is respected.

Not all panes support output recording, but certainly application panes
do, so it is good to use some subclass of `application-pane` to display
application-specific object, because output recording is then automatic.

### An application displaying a data structure

Many applications use a central data structure that is to be on display
at all times, and that is modified by the commands of the application.
CLIM allows for a very easy way to write such an application. The main
idea is to store the data structure in slots of the application frame,
and to use a *display function* that after each iteration of the command
loop displays the entire data structure to the application pane.

Here is a variation of the previous application that shows this
possibility where the `parity` is now useful.

```commonlisp
(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()

  ;; New addition of a slot to the application frame which
  ;; defines a application-specific slot.  

  ;; The slot is simply a number.
  ((currrent-number :initform nil
		    :accessor current-number))

  ;; The rest of the application frame is unchanged.
  (:pointer-documentation t)
  (:panes
    (app :application
	 :height 400
	 :width 600
	 :display-function 'display-app)
    (int :interactor
	 :height 200
	 :width 600))
  (:layouts
    (default (vertically ()
	      app int))))

;; This is the function that will display the pane app.
;; Simply prints the number of the application frame slot
;; and whether it is odd or even.
;; Note that the print stream of format is pane.
(defun display-app (frame pane)
  (let ((number (current-number frame)))
    (format pane "~a is ~a"
	    number
	    (cond ((null number) "not a number")
		  ((oddp number) "odd")
		  (t "even")))))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-superapp-command (com-parity :name t) ((number 'integer))
  (setf (current-number *application-frame*) number))


(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))
```

Here, we have added a slot that is called `current-number` to the
application frame. It is initialized to `NIL` and it has an accessor
function that allow us to query and to modify the value.

Observe that in this example, we no longer have the option
`:display-time nil` set in the application pane. By default, then, the
`:display-time` is `:command-loop` which means that the pane is erased
after each iteration of the command loop. Also observe the option
`:display-function` which takes a symbol that names a function to be
called to display the pane after it has been cleared. In this case, the
name is `display-app`, the name of the function defined immediately
after the application frame.

Instead of immediately displaying information about its argument, the
command `com-parity` instead modifies the new slot of the application
frame. Think of this function as being more general, for instance a
command to add a new object to a set of graphical objects in a figure
drawing program, or as a command to add a new name to an address book.
Notice how this function accesses the current application frame by means
of the special variable `*application-frame*`.

A display function is called with the frame and the pane as arguments.
It is good style to use the pane as the stream in calls to functions
that will result in output. (Recall that a pane is a stream of rendering
instructions.) This makes it possible for the same function to be used
by several different frames, should that be called for. In our simple
example, the display function only displays the value of a single number
(or `NIL`), but you could think of this as displaying all the objects
that have been drawn in some figure drawing program or displaying all
the entries in an address book.

### Incremental redisplay
CLIM applications are most often structured around the *command loop*.
The various steps that such an application follow are:

-   A *command* is acquired because the user, either typed the name of
    the command to an interactive prompt, selected a menu item
    representing a command, or clicked on an active object that
    translates to a command.

-   The *arguments* to that command are acquired. As with the
    acquisition of the command itself, various gestures can be used to
    supply the arguments.

-   The command is *executed* with the acquired arguments. Typically,
    the command modifies some part of the *model* contained in one or
    more slots in the application frame ^[Some authors use the term
    *business logic* instead of *model*. Both words refer to the
    representation of the intrinsic purpose of the application, as
    opposed to superficial characteristics such as how objects are
    physically presented to the user.].

-   The *redisplay functions* associated with the visible panes of the
    application are executed. Typically, the redisplay function erases
    all the output and traverses the entire model in order to produce a
    new version of that output. Since output exists in the form of
    *output records*, this process involves deleting the existing output
    records and computing an entirely new set of them.

This way of structuring an application is very simple. The resulting
code is very easy to understand, and the relationship between the code
of a redisplay function and the output it produces is usually obvious.
The concept of output records storing the output in the application pane
is completely hidden, and instead output is produced using textual or
graphic drawing functions, or more often produced indirectly through the
use of `present` or `with-output-as-presentation`.

However, if the model contains a large number of objects, then this
simple way of structuring an application may penalize performance. In
most libraries for creating graphic user interfaces, the application
programmer must then rewrite the code for manipulating the model, and
especially for incrementally altering the output according to the
modification of the model resulting from the execution of a command.

In CLIM, a different mechanism is provided called *incremental
redisplay*. This mechanism allows the user to preserve the simple logic
of the display function with only minor modifications while still being
able to benefit in terms of performance.

While the example in the previous section is a very simple way of
structuring an application (let commands arbitrarily modify the data
structure, and simply erase the pane and redisplay the structure after
each iteration of the command loop), the visual result is not so great
when many objects are to be displayed. There is most often a noticeable
flicker between the moment when the pane is cleared and the objects are
drawn. Sometimes this is inevitable (as when nearly all objects change),
but most of the time, only an incremental modification has been made,
and most of the objects are still in the same place as before. E.g. no
point refreshing a whole spreadsheet when only a single cell's content
has changed.

In simpler GUI toolkits, the application programmer would have to
provide code logic to explicitly track what might changed since the
previous display, and only display the differences. CLIM offers a
mechanism called *incremental redisplay* that automates a large part
(but not all) of this task. As we mentioned earlier, CLIM can
transparently and continuously capture output in the form of *output
records*. The same mechanism is used to obtain incremental redisplay.

The incremental redisplay code of a pane remains structured as in the
previous section: after each iteration of the command loop, the display
function needs to produce an output (the rendering instructions) of the
entire data structure. As before, the new (incremental) display code
will iterate through each individual element of that pane, but this time
with a way to test whether this element has changed. The mechanics of
updating the internal structure of the output, changing only needs to be
changed, is then fully handled by CLIM: instead of regenerating the
entire stream of instructions to render a particular pane, only the
instructions of a modified element of that pane will be updated and only
part of that stream of instructions will be modified.

To achieve this, each element of that pane is given a unique identifier,
as well as a way to compare the value referenced by that identifier
before and after a redisplay request. The incremental redisplay
mechanism then works by telling CLIM which piece of output corresponds
to which piece of output during the previous iteration of the command
loop. With this information, the CLIM incremental redisplay mechanism
can figure out whether some output is new, has disappeared, or has been
moved, compared to the previous iteration of the command loop. As with
re-exposure, CLIM guarantees that the result is identical to that which
would have been obtained, had all the output records been output in
order to a blank pane.

The next example illustrates this idea. It is a simple application that
displays line-by-line a list of random numbers (here 20). The user is
then able to move a cursor represented by a star at the beginning of a
line, and increase or decrease the number on that line by 1.

When displaying those lines (displaying the first time or refreshing the
display afterwards), the code will identify which has changed or not,
and only update the `output record` (the list of rendering instructions)
if a number changed. Concretely, moving the star cursor up or down does
not modify the number and therefore should not affect the
`output record` of that number; this should only happen when a number is
increased or decreased. The star cursor however will always be
re-rendered without using the incrementaly redisplay mechanics.

Here is the code achieving this:

```commonlisp
(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()
  ((numbers :initform (loop repeat 20 collect (list (random 100000000)))
	    :accessor numbers)
   (cursor :initform 0 :accessor cursor))
  (:pointer-documentation t)
  (:panes
    (app :application
	 :height 400 :width 600
	 :incremental-redisplay t
	 :display-function 'display-app)
    (int :interactor :height 200 :width 600))
  (:layouts
    (default (vertically () app int))))

;; As usual, the displaying code relates to a pane, not the application frame. 
(defun display-app (frame pane)

  (loop
     ;; taking items one-by-one from the frame slot 'numbers'
     for current-element in (numbers frame)

     ;; and increasing line-by-line  
     for line from 0

     ;; prints a star if the cursor is on that line
     ;; (Note that here, there is no incremental redisplay. The output
     ;; record of the star will be printed at each call of the display
     ;; function -- that is at each iteration of the command loop.)
     do (princ (if (= (cursor frame) line) "*" " ") pane)

     ;; and incrementally updates the rendering instructions of the
     ;; number on that line
     ;; (Note that 'numbers' was defined as a list of lists, each
     ;; sublist holding an individual number. The reason for that is
     ;; explained below, but this is why (car current-element) is
     ;; needed.)
     do (updating-output (pane :unique-id   current-element
			       :id-test     #'eq
			       :cache-value (car current-element)
			       :cache-test  #'eql)
	  (format pane "~a~%" (car current-element)))))


;;
;; Command definitions
;;

;; increase the value of the number on the current line
(define-superapp-command (com-add :name t) ((number 'integer))
  (incf (car (elt (numbers *application-frame*)
		  (cursor *application-frame*)))
	number))

;; move the cursor one line down (increasing the cursor position),
;; looping back to the beginning if going too far
(define-superapp-command (com-next :name t) ()
  (incf (cursor *application-frame*))
  (when (= (cursor *application-frame*)
	   (length (numbers *application-frame*)))
    (setf (cursor *application-frame*) 0)))

;; move the cursor one line up
(define-superapp-command (com-previous :name t) ()
  (decf (cursor *application-frame*))
  (when (minusp (cursor *application-frame*))
    (setf (cursor *application-frame*)
	  (1- (length (numbers *application-frame*))))))

;; Command to quit the app
(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))


;; Exported function to launch an instance of the application frame 
(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))
```

We store the numbers in a slot called `numbers` of the application
frame. However, we store each number in its own list. This is a simple
way to provide a unique identity for each number. We could not use the
number itself, because two numbers could be the same. Since numbers
evaluate to themselves, the identities would not be unique. Instead, we
use the cons cell that store the number as the unique identity. By using
`:id-test éq` we inform CLIM that it can figure out whether an output
record is the same as one that was issued previous time by using the
function `eq` to compare them. But there is a second test that has to be
verified, namely whether an output record that was issued last time has
to be redisplayed or not. That is the purpose of the cache-value. Here
we use the number itself as the cache value and `eql` as the test to
determine whether the output is going to be the same as last time.

For convenience, we display a `*` at the beginning of the current line,
and we provide two commands `next` and `previous` to navigate between
the lines. The command `add` increases the value of the number at the
cursor position.

Notice that in the declaration of the pane in the application frame, we
have given the option `:incremental-redisplay t`. This informs CLIM not
to clear the pane after each command-loop iteration, but to keep the
output records around and compare them to the new ones that are produced
during the new iteration.
