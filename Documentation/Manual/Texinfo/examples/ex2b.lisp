(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()
  ;; New addition of a slot to the application frame, which defines a
  ;; application-specific slot. The slot is simply a number.
  ((current-number :initform nil
                    :accessor current-number))

  ;; We no longer specify :DISPLAY-TIME (see below), and we supply our
  ;; own :DISPLAY-FUNCTION. The rest of the application frame is
  ;; unchanged.
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

;; The display function for the "app" pane. It simply prints the
;; number from the application frame slot, and whether it is odd or
;; even. Note that the print stream of `format' is PANE.
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
