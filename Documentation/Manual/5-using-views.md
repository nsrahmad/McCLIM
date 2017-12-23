## Using views

The CLIM specification mentions a concept called a *view*, and also
lists a number of predefined views to be used in various different
contexts.

In this chapter we show how the *view* concept can be used in some
concrete programming examples. In particular, we show how to use a
single pane to show different views of the application data structure at
different times. To switch between the different views, we supply a set
of commands that alter the `stream-default-view` feature of all CLIM
extended output streams.

The example shown here has been stripped to a bare minimum in order to
illustrate the important concepts. A more complete version can be found
in `Examples/views.lisp` in the McCLIM source tree.

Here is the example:

```commonlisp
;;; part of application "business logic"
(defclass person ()
  ((%last-name :initarg :last-name :accessor last-name)
   (%first-name :initarg :first-name :accessor first-name)
   (%address :initarg :address :accessor address)
   (%membership-number :initarg :membership-number :reader membership-number)))

;;; constructor for the PERSON class.  Not strictly necessary. 
(defun make-person (last-name first-name address membership-number)
  (make-instance 'person 
                 :last-name last-name 
                 :first-name first-name
                 :address address
                 :membership-number membership-number))

;;; initial list of members of the organization we imagine for this example
(defparameter *members*
  (list (make-person "Doe" "Jane" "123, Glencoe Terrace" 12345)
        (make-person "Dupont" "Jean" "111, Rue de la Republique" 54321)
        (make-person "Smith" "Eliza" "22, Trafalgar Square" 121212)
        (make-person "Nilsson" "Sven" "Uppsalagatan 33" 98765)))

;;; the CLIM view class that corresponds to a list of members, one member
;;; per line of text in a CLIM application pane. 
(defclass members-view (view) ())

;;; since this view does not take any parameters in our simple example,
;;; we need only a single instance of it. 
(defparameter *members-view* (make-instance 'members-view))

;;; the application frame.  It contains instance-specific data
;;; such as the members of our organization. 
(define-application-frame views ()
  ((%members :initform *members* :accessor members))
  (:panes
   (main-pane :application :height 500 :width 500
              :display-function 'display-main-pane
              ;; notice the initialization of the default view of
              ;; the application pane. 
              :default-view *members-view*)
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default (vertically ()
              main-pane
              interactor))))

;;; the trick here is to define a generic display function
;;; that is called on the frame, the pane AND the view, 
;;; whereas the standard CLIM display functions are called 
;;; only on the frame and the pane.
(defgeneric display-pane-with-view (frame pane view))

;;; this is the display function that is called in each iteration
;;; of the CLIM command loop.  We simply call our own, more elaborate
;;; display function with the default view of the pane. 
(defun display-main-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

;;; now we can start writing methods on our own display function
;;; for different views.  This one displays the data each member
;;; on a line of its own.
(defmethod display-pane-with-view (frame pane (view members-view))
  (loop for member in (members frame)
        do (with-output-as-presentation 
               (pane member 'person)
             (format pane "~a, ~a, ~a, ~a~%"
                     (membership-number member)
                     (last-name member)
                     (first-name member)
                     (address member)))))

;;; this CLIM view is used to display the information about
;;; a single person.  It has a slot that indicates what person
;;; we want to view. 
(defclass person-view (view)
  ((%person :initarg :person :reader person)))

;;; this method on our own display function shows the detailed 
;;; information of a single member. 
(defmethod display-pane-with-view (frame pane (view person-view))
  (let ((person (person view)))
    (format pane "Last name: ~a~%First Name: ~a~%Address: ~a~%Membership Number: ~a~%"
            (last-name person)
            (first-name person)
            (address person)
            (membership-number person))))

;;; entry point to start our applciation
(defun views-example ()
  (run-frame-top-level (make-application-frame 'views)))

;;; command to quit the application 
(define-views-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;;; command to switch the default view of the application pane
;;; (which is the value of *standard-output*) to the one that
;;; shows a member per line. 
(define-views-command (com-show-all :name t) ()
  (setf (stream-default-view *standard-output*) *members-view*))
    
;;; command to switch to a view that displays a single member. 
;;; this command takes as an argument the person to display.  
;;; In this application, the only way to satisfy the demand for
;;; the argument is to click on a line of the members view.  In 
;;; more elaborate application, you might be able to type a
;;; textual representation (using completion) of the person. 
(define-views-command (com-show-person :name t) ((person 'person))
  (setf (stream-default-view *standard-output*)
        (make-instance 'person-view :person person)))
```

The example shows a stripped-down example of a simple database of
members of some organization.

The main trick used in this example is the `display-main-pane` function
that is declared to be the display function of the main pane in the
application frame. The `display-main-pane` function trampolines to a
generic function called `display-pane-with-view`, and which takes an
additional argument compared to the display functions of CLIM panes.
This additional argument is of type `view` which allows us to dispatch
not only on the type of frame and the type of pane, but also on the type
of the current default view. In this example the view argument is simply
taken from the default view of the pane.

A possibility that is not obvious from reading the CLIM specification is
to have views that contain additional slots. Our example defines two
subclasses of the CLIM `view` class, namely `members-view` and
`person-view`.

The first one of these does not contain any additional slots, and is
used when a global view of the members of our organization is wanted.
Since no instance-specific data is required in this view, we follow the
idea of the examples of the CLIM specification to instantiate a
singleton of this class and store that singleton in the
`stream-default-view` of our main pane whenever a global view of our
organization is required.

The `person-view` class, on the other hand, is used when we want a
closer view of a single member of the organization. This class therefore
contains an additional slot which holds the particular person instance
we are interested in. The method on `display-pane-with-view` that
specializes on `person-view` displays the data of the particular person
that is contained in the view.

To switch between the views, we provide two commands. The command
`com-show-all` simply changes the default view of the main pane to be
the singleton instance of the `members-view` class. The command
`com-show-person` is more complicated. It takes an argument of type
person, creates an instance of the `person-view` class initialized with
the person that was passed as an argument, and stores the instance as
the default view of the main pane.
