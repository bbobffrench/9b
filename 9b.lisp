;;;; 9b.lisp

(defpackage 9b (:use common-lisp 9b/io) (:export main))
(in-package 9b)

(defvar *width* 600)
(defvar *height* 600)

; (9b/io:init-window 600 600 "/usr/share/fonts/truetype/iosevka/iosevka-regular.ttf" 12)

(defun main ()
  (setf ft2:*library* (ft2:make-freetype))
  (init-window *width* *height* "/usr/share/fonts/truetype/iosevka/iosevka-regular.ttf" 12)
  (do ((event (get-event) (get-event)))
      ((eq (car event) :destroy-request))
    (if (eq (car event) :resize)
        (setf *width* (cdr (assoc :width (cdr event)))
              *height* (cdr (assoc :height (cdr event)))))
    (if (eq (car event) :exposure)
        (progn (draw-rect '(#xff #xff #xff) 0 0 *width* *height*)
               (draw-text '(0 0 0) "Hello, world!" 100 100)
               (flush)))
    (print event)
    (force-output))
  (destroy-window))
