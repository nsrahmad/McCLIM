# Developer Manual

## Writing backends {#Writing-backends}

### Different types of backends {#Different-types-of-backends}
Backend provides platform specific API for low level drawing operations,
getting events, managing window geometry properties and providing native
look-and-feel to the application.

There are three types of backends:

Draw-only backend

:   This type doesn't implement any kind of events and allows only
    drawing on it. A good example of it is the See [PostScript
    Backend](#PostScript-Backend) which is part of *CLIM II*
    specification.

Basic backend

:   OpenGL, X, or HTML 5 canvas are resources which provide only drawing
    and event handling primitives. In this case we need to wrap their
    APIs for McCLIM to use. McCLIM will then use these drawing and
    windowing primitives to implement portable widgets.

Native backend

:   Native backend is based on already complete GUI library which
    provides a rich set of widgets (for example Cocoa or Win32 API).
    Additionally to the things needed to be implement in the first two
    cases, we can also map these native look and feel widgets in McCLIM.

The `clim-null` backend can be used as a template to start with a new
backend. If the underlying library you write backend for manages window
hierarchy, positioning and events, it is possible to base new pane types
on `clim-standard:standard-full-mirrored-sheet-mixin`. Otherwise
`clim-standard:standard-single-mirrored-sheet-mixin` provides
calculation to support that hierarchy in Lisp side.

**Backend protocol**

    NEW CLASS FOR BACKEND `FOO'
    ---------------------------
    foo-frame-manager
    foo-native-frame-manager (optional)
    foo-graft
    foo-port
    foo-medium
    foo-pointer

**Event handling**

    EVENT HANDLING (in port.lisp)
    -----------------------------
    ;;; Use clim-standard:standard-event-port-mixin with
    ;;; clim-standard:standard-full-mirrored-sheet-mixin
    ;;;
    ;;; Use clim-standard:standard-handled-event-port-mixin with
    ;;; clim-standard:standard-single-mirrored-sheet-mixin
    ;;;
    ;;; and make this event-port-mixin as a subclass of foo-port

    ;;; Originally in CLIM-INTERNALS
    get-next-event
    port-frame-keyboard-input-focus
    port-grab-pointer
    port-ungrab-pointer
    synthesize-pointer-motion-event

**Graft protocol**

    GRAFT (in grafts.lisp)
    -----------------------------------
    ;;; Use clim-standard:standard-graft as superclass

    ;;; Originally in CLIM
    graft        ; root window/screen
    graft-height ; screen height
    graft-width  ; screen width

**Medium drawing**

    MEDIUM DRAWING (in medium.lisp)
    -------------------------------
    ;;; Originally in CLIM
    medium-draw-circle*
    medium-draw-ellipse*
    medium-draw-line*
    medium-draw-lines*
    medium-draw-point*
    medium-draw-points*
    medium-draw-polygon*
    medium-draw-rectangle*
    medium-draw-rectangles*
    medium-draw-text*

**Medium operationMiscellanies protocol**

    MEDIUM OPERATIONS (in medium.lisp)
    ----------------------------------
    ;;; Originally in CLIM
    make-medium ; make medium for a given sheet
    medium-beep
    medium-buffering-output-p
    medium-clear-area
    medium-copy-area
    medium-finish-output
    medium-force-output
    medium-line-style
    medium-text-style

**Port protocol**

    PORT (BRIDGE) TO GUI (A SERVER LIKE)
    ------------------------------------
    ;;; Originally in CLIM
    destroy-port

    ;;; Originally in CLIM-INTERNALS
    port-allocate-pixmap
    port-deallocate-pixmap
    port-disable-sheet
    port-enable-sheet
    port-force-output
    port-mirror-height
    port-mirror-width
    port-set-mirror-region
    port-set-mirror-transformation
    set-sheet-pointer-cursor

**Frame manager, panes and gadgets**

    FRAME MANAGER, PANES AND GADGETS
    --------------------------------
    ;;; Originally in CLIM
    ;; in frame-manager.lisp
    make-pane-1
    note-space-requirements-changed
    adopt-frame

    ;; in port.lisp or pane.lisp/gadget.lisp
    allocate-space
    destroy-mirror
    handle-repaint
    realize-mirror

**Pointer protocol (events?)**

    POINTER (port.lisp or pointer.lisp)
    -----------------------------------
    ;;; Originally in CLIM
    pointer-button-state
    pointer-modifier-state
    pointer-position

**Text size**

    TEXT SIZE (medium.lisp)
    -----------------------
    ;;; Originally in CLIM-INTERNALS
    text-style-character-width
    ;;; Originally in CLIM
    text-size
    text-style-ascent
    text-style-descent
    text-style-height
    text-style-mapping
    text-style-width

**Text selection**

    TEXT SELECTION (port.lisp)
    --------------------------
    ;;; Originally in CLIM
    selection-owner
    selection-timestamp
    selection-event
    selection-clear-event
    selection-notify-event
    selection-request-event
    selection-event-requestor
    request-selection
    release-selection
    bind-selection
    send-selection
    get-selection-from-event

**Miscellaneous**

    MISC
    ----
    ;;; Originally in CLIM-INTERNALS
    invoke-with-special-choices ; not sure, seems just a funcall

    ;;; Originally in CLIM-EXTENSIONS
    medium-miter-limit          ; determine a draw for miter < sina/2

**Obsolete**

    NO LONGER NEEDED IN BACKEND
    ---------------------------
    medium-draw-glyph               ; X-specific concept
    port-motion-hints               ; X-specific concept
    queue-callback                  ; moved to clim-core
    medium-clipping-                ; moved to clim-basic
    port-set-sheet-region           ; never used
    port-set-sheet-transformation   ; never used
    mirror-transformation           ; never used
