## Using presentation types

### What is a presentation type

The concept of *presentation types* is central to CLIM. Client code can
choose to output graphical or textual representations of application
objects either as just graphics or text, or to associate such output
with an arbitrary Common Lisp object and a presentation type. The
presentation type is not necessarily related to the idea Common Lisp
might have of the underlying object.

When a CLIM command or some other client code requests an object (say as
an argument) of a certain presentation type, the user of the application
can satisfy the request by clicking on any visible output labeled with a
compatible presentation type. The command then receives the underlying
Common Lisp object as a response to the request.

CLIM presentation types are usually distinct from Common Lisp types. The
reason is that the Common Lisp type system, although very powerful, is
not quite powerful enough to represent the kind of relationships between
types that are required by CLIM. However, every Common Lisp class
(except the built-in classes) is automatically a presentation type.

A presentation type has a name, but can also have one or more
*parameters*. Parameters of presentation types are typically used to
restrict the type. For instance, the presentation type `integer` takes
as parameters the low and the high values of an interval. Such
parameters allow the application to restrict objects that become
clickable in certain contexts, for instance if a date in the month of
March is requested, only integers between 1 and 31 should be clickable.

### A simple example

Consider the following example:

```commonlisp
(in-package :common-lisp-user)

(defpackage :app
  (:use :clim :clim-lisp)
  (:export #:app-main))

(in-package :app)

(define-application-frame superapp ()
  ()
  (:pointer-documentation t)
  (:panes
    (app :application :display-time t :height 300 :width 600)
    (int :interactor :height 200 :width 600))
  (:layouts
    (default (vertically () app int))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-type name-of-month ()
  :inherit-from 'string)

(define-presentation-type day-of-month ()
  :inherit-from 'integer)

(define-superapp-command (com-out :name t) ()
  (with-output-as-presentation (t "The third month" 'name-of-month)
    (format t "March~%"))
  (with-output-as-presentation (t 15 'day-of-month)
    (format t "fifteen~%")))

(define-superapp-command (com-get-date :name t)
    ((name 'name-of-month) (date 'day-of-month))
  (format (frame-standard-input *application-frame*)
	  "the ~a of ~a~%" date name))
```

In this application, we have two main panes, an application pane and an
interactor pane. The application pane is given the option
`:display-time t` which means that it will not be erased before every
iteration of the command loop.

We have also defined two presentation types: `name-of-month` and
`day-of-month`. The `out` command uses `with-output-as-presentation` in
order to associate some output, a presentation type, and an underlying
object. In this case, it will show the string "March" which is
considered to be of presentation type `name-of-month` with the
underlying object being the character string `"The third month"`. It
will also show the string "fifteen" which is considered to be of
presentation type `day-of-month` with the underlying object being the
number 15. The argument `t` to `with-output-as-presentation` indicates
that the stream to present on is `*standard-output*`.

Thus, if the `out` command has been executed, and then the user types
"Get Date" in the interactor pane, the `get-date` command will try to
acquire its arguments, the first of presentation type `name-of-month`
and the second of type `day-of-month`. At the first prompt, the user can
click on the string "March" but not on the string "fifteen" in the
application pane. At the second prompt it is the string "fifteen" that
is clickable, whereas "March" is not.

The `get-date` command will acquire the underlying objects. What is
finally displayed (in the interactor pane, which is the standard input
of the frame), is "the 15 of The third month".
