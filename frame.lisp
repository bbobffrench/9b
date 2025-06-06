;;;; frame.lisp
;;;;
;;;; This file implements the concept of a frame. Frames are comprised of four compenents: title
;;;; buffer, main buffer, scroll bar, and corner. Frames are stateless, and only hold information
;;;; on their current dimensions and coordinates.

(defpackage 9b/frame
  (:use :common-lisp :9b/utils :9b/colors :9b/sys-io)
  (:export *scroll-bar-width*
           make-frame
           draw-main-buffer
           draw-title-buffer
           draw-corner
           draw-scroll-bar
           draw-buffer-sep
           draw-frame-sep
           draw-frame))

(in-package :9b/frame)

(defparameter *scroll-bar-width* 14)

(defun make-frame (x y width height)
  (list (cons :x x) (cons :y y) (cons :width width) (cons :height height)))

;;; Drawing of the main frame components
;;;
(defun draw-rect (frame x y width height color)
  (set-color color)
  (alist-bind ((:x frame-x) (:y frame-y))
      frame
    (let ((start-x (+ x frame-x)) (start-y (+ y frame-y)))
      (move-to start-x start-y)
      (line-to (+ start-x width) start-y)
      (line-to (+ start-x width) (+ start-y height))
      (line-to start-x (+ start-y height))))
  (fill-region))

(defmacro defdraw (name args &body body)
  `(defun ,name ,(append args '(&optional display-p))
     ,@body
     (if display-p (display-window))))

(defdraw draw-main-buffer (frame)
  (alist-bind ((:width frame-width) (:height frame-height))
      frame
    ;; Account for scroll bar and title buffer
    (let* ((start-x *scroll-bar-width*) (width (- frame-width start-x))
           (start-y (+ (glyph-height) 3)) (height (- frame-height start-y)))
      (draw-rect frame
                 start-x start-y
                 width height
                 *bg-color*))))

(defdraw draw-title-buffer (frame )
  (alist-bind ((:width frame-width))
      frame
    (draw-rect frame
               *scroll-bar-width* 0
               (- frame-width *scroll-bar-width*) (+ (glyph-height) 2)
               *title-buffer-bg-color*)))

(defdraw draw-corner (frame style)
  (draw-rect frame
             0 0
             *scroll-bar-width* (+ (glyph-height) 3)
             (if (eq style :top-title) *title-buffer-bg-color* *corner-bg-color*))
  (draw-rect frame
             2 2
             (- *scroll-bar-width* 4) (- (+ (glyph-height) 2) 3)
             (case style
             (:top-title *title-buffer-bg-color*)
             (:column-title *corner-bg-color*)
             (:unmodified *corner-unmodified-color*)
             (:modified *corner-modified-color*))))

(defdraw draw-scroll-bar (frame start% fill%)
  (alist-bind ((:height frame-height))
      frame
    (let ((start-y (+ (glyph-height) 3))) ; Account for title buffer height
      (draw-rect frame
                 0 start-y
                 *scroll-bar-width* (- frame-height start-y)
                 *scroll-bar-bg-color*)
      ;; Find the size and dimensions of the active portion based on start and fill percentages
      (let ((active-start-y (+ start-y (ceiling (* start% frame-height))))
            (active-height (ceiling (* fill% (- frame-height start-y)))))
        (draw-rect frame
                   0 active-start-y
                   (1- *scroll-bar-width*) active-height
                   *scroll-bar-fg-color*)))))

(defdraw draw-buffer-sep (frame)
  (alist-bind ((:width frame-width))
      frame
    (draw-rect frame
               0 (+ (glyph-height) 2)
               frame-width 1
               *title-buffer-sep-color*)))

(defdraw draw-sep (frame pos vertical-p)
  (alist-bind ((:width frame-width) (:height frame-height))
      frame
    (if vertical-p
        (draw-rect frame pos 0 2 frame-height *fg-color*)
        (draw-rect frame 0 pos frame-width 2 *fg-color*))))

(defdraw draw-frame (frame start% fill% corner-style)
  (draw-title-buffer frame)
  (draw-corner frame corner-style)
  (draw-scroll-bar frame start% fill%)
  (draw-main-buffer frame)
  (draw-buffer-sep frame))