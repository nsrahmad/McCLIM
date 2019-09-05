;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-FREETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: TrueType font detection
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2008 by Andy Hefner
;;;  (c) copyright 2016 by Daniel Kochmański
;;;
;;;    See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :mcclim-truetype)

;;; fallback (path may be set in a restart by the user)
(defparameter *truetype-font-path* (clime:asset :mcclim "ttf/"))

;;; Here are mappings for the DejaVu family of fonts, which are a
;;; derivative of Vera with improved unicode coverage.
;;;
;;; Paths are relative so we are able to rely on value of a special
;;; variable *truetype-font-path*, so if it is changed in
;;; `invoke-with-truetype-path-restart' it will be used.

(defvar *families/faces*
  '(((:fix :roman) . "DejaVuSansMono.ttf")
    ((:fix :italic) . "DejaVuSansMono-Oblique.ttf")
    ((:fix (:bold :italic)) . "DejaVuSansMono-BoldOblique.ttf")
    ((:fix (:italic :bold)) . "DejaVuSansMono-BoldOblique.ttf")
    ((:fix :bold) . "DejaVuSansMono-Bold.ttf")
    ((:serif :roman) . "DejaVuSerif.ttf")
    ((:serif :italic) . "DejaVuSerif-Italic.ttf")
    ((:serif (:bold :italic)) . "DejaVuSerif-BoldItalic.ttf")
    ((:serif (:italic :bold)) . "DejaVuSerif-BoldItalic.ttf")
    ((:serif :bold) . "DejaVuSerif-Bold.ttf")
    ((:sans-serif :roman) . "DejaVuSans.ttf")
    ((:sans-serif :italic) . "DejaVuSans-Oblique.ttf")
    ((:sans-serif (:bold :italic)) . "DejaVuSans-BoldOblique.ttf")
    ((:sans-serif (:italic :bold)) . "DejaVuSans-BoldOblique.ttf")
    ((:sans-serif :bold) . "DejaVuSans-Bold.ttf")))

(defun invoke-with-truetype-path-restart (continuation)
  (restart-case (funcall continuation)
    (change-font-path (new-path)
      :report (lambda (stream) (format stream "Retry with alternate truetype font path"))
      :interactive (lambda ()
                     (format *query-io* "Enter new value: ")
                     (list (read-line)))
      (setf *truetype-font-path* new-path)
      (invoke-with-truetype-path-restart continuation))))

;;; predefined paths (registers all found ttf fonts)

(defun default-font/family-map ()
  (flet ((try-ttf (name)
           ;; probe for files existance - if they do not exist our
           ;; mapping is futile and we must try `fc-match'.
           (if-let ((path (probe-file
			   (merge-pathnames name *truetype-font-path*))))
	     path
	     (progn
	       (warn "~s doesn't exist" (merge-pathnames name *truetype-font-path*))
	       (return-from default-font/family-map)))))
    `(((:fix :roman) .                 ,(try-ttf "DejaVuSansMono.ttf" ))
      ((:fix :italic) .                ,(try-ttf "DejaVuSansMono-Oblique.ttf"))
      ((:fix (:bold :italic)) .        ,(try-ttf "DejaVuSansMono-BoldOblique.ttf"))
      ((:fix (:italic :bold)) .        ,(try-ttf "DejaVuSansMono-BoldOblique.ttf"))
      ((:fix :bold) .                  ,(try-ttf "DejaVuSansMono-Bold.ttf"))
      ((:serif :roman) .               ,(try-ttf "DejaVuSerif.ttf"))
      ((:serif :italic) .              ,(try-ttf "DejaVuSerif-Italic.ttf"))
      ((:serif (:bold :italic)) .      ,(try-ttf "DejaVuSerif-BoldItalic.ttf"))
      ((:serif (:italic :bold)) .      ,(try-ttf "DejaVuSerif-BoldItalic.ttf"))
      ((:serif :bold) .                ,(try-ttf "DejaVuSerif-Bold.ttf"))
      ((:sans-serif :roman) .          ,(try-ttf "DejaVuSans.ttf"))
      ((:sans-serif :italic) .         ,(try-ttf "DejaVuSans-Oblique.ttf"))
      ((:sans-serif (:bold :italic)) . ,(try-ttf "DejaVuSans-BoldOblique.ttf"))
      ((:sans-serif (:italic :bold)) . ,(try-ttf "DejaVuSans-BoldOblique.ttf"))
      ((:sans-serif :bold) .           ,(try-ttf "DejaVuSans-Bold.ttf")))))

;;; configure fonts
(defun autoconfigure-fonts ()
  (invoke-with-truetype-path-restart
   (lambda ()
     (check-type *truetype-font-path* pathname)
     (let ((map (default-font/family-map)))
       (setf *families/faces* map)))))
