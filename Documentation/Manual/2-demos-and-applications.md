# User Manual

## CLIM demos and applications

### Running the demos

The McCLIM source distribution comes with a number of demos and
applications. They are intended to showcase specific CLIM features,
demonstrate programming techniques or provide useful tools.

These demos and applications are available in the `Examples` and `Apps`
subdirectories of the source tree's root directory. Instructions for
compiling, loading and running some of the demos are included in the
file `INSTALL` with the McCLIM installation instructions for your Common
Lisp implementation.

Below is a complete list of the McCLIM demos and applications, sorted in
alphabetical order. Each entry provides a short description of what the
program does, with instructions for compiling and running it if not
mentioned in the general installation instructions.

Demos are meant to be run after loading the `clim-examples` system from
the frame created with `(clim-demo:demodemo)`.

The easiest way to try this is to use *Quicklisp* library manager.
Assuming that this is already setup, trying out the demos is
straightforward:

```commonlisp
    (ql:quickload 'clim-examples)
    (clim-demo:demodemo)
```

Alternatively, for the more courageous (which requires separately
downloading dependencies and setting a local repository), `asdf` also
works well starting from the McCLIM source code directory.

```commonlisp
    (asdf:load-system 'clim-examples)
    (clim-demo:demodemo)
```

Available demos and tests are defined in the following files:

-   Examples/demodemo.lisp

    Demonstrates different pane types and other tests.

-   Examples/clim-fig.lisp

    Simple paint program.

-   Examples/calculator.lisp

    Simple desk calculator.

-   Examples/method-browser.lisp

    Example of how to write a CLIM application with a "normal" GUI,
    where "normal" is a completely event driven app built using gadgets
    and not using the command-oriented framework.

-   Examples/address-book.lisp

    Simple address book.

-   Examples/puzzle.lisp

    Simple puzzle game.

-   Examples/colorslider.lisp

    Interactive color editor.

-   Examples/town-example.lisp

    "Large Cities of Germany" application example by Max-Gerd Retzlaff.

-   Examples/logic-cube.lisp

    Software-rendered 3d logic cube game. Shows how the transformations
    work and how to implement custom handle-repaint methods.

-   Examples/menutest.lisp

    Displays a window with a simple menu bar.

-   Examples/gadget-test.lisp

    Displays a window with various gadgets.

-   Examples/dragndrop.lisp

    Example of "Drag and Drop" functionality.

-   Examples/dragndrop-translator.lisp

    Another example of "Drag and Drop" functionality (with colors!).

-   Examples/draggable-graph.lisp

    Demo of draggable graph nodes.

-   Examples/image-viewer.lisp

    A simple program for displaying images of formats known to McCLIM.

-   Examples/font-selection.lisp

    A font selection dialog.

-   Examples/tabdemo.lisp

    A tab layout demo (McCLIM extension).

-   Examples/postscript-test.lisp

    Displays text and graphics to a PostScript file. Run it with:

        (clim-demo::postscript-test)

    The resulting file `ps-test.ps` is generated in the current
    directory and can be displayed by a PostScript viewer such as `gv`
    on Unix-like systems.

-   Examples/presentation-test.lisp

    Displays an interactive window in which you type numbers that are
    successively added. When a number is expected as input, you can
    either type it at the keyboard, or click on a previously entered
    number. Labeled "Summation".

-   Examples/sliderdemo.lisp

    Apparently a calculator demo (see above). Labeled "Slider demo".

-   Examples/stream-test.lisp

    Interactive command processor that echoes its input in
    `*trace-output*`.

The following programs are currently **known not to work**:

-   `Examples/gadget-test-kr.lisp`

-   `Examples/traffic-lights.lisp`

### Applications

-   Apps/Listener

    CLIM-enabled Lisp listener. System name is `clim-listener`. See
    instructions in `Apps/Listener/README` for more information.

-   Apps/Inspector

    CLIM-enabled Lisp inspector. System name is `clouseau`. See
    instructions in `Apps/Inspector/INSTALL` for more information..

-   Apps/Debugger

    Common Lisp debugger implemented in McCLIM. It uses the portable
    debugger interface developed for the Slime project. Application has
    some quirks and requires work. System name is `clim-debugger`.

-   Apps/Functional-Geometry

    Peter Henderson idea, see http://www.ecs.soton.ac.uk/ ph/funcgeo.pdf
    and http://www.ecs.soton.ac.uk/ ph/papers/funcgeo2.pdf implemented
    in Lisp by Frank Buss. CLIM Listener interface by Rainer Joswig.
    System name is `functional-geometry`.

```commonlisp
(functional-geometry:run-functional-geometry)
(clim-plot *fishes*) ; from a listener
```

-   Apps/Scigraph

    Scigraph Scientific Graphing Package. See the compilation and
    execution instructions in `Apps/Scigraph/README`.
