;;;; frame.lisp
;;;;
;;;; This file implements the concept of a frame. A frame has no knowledge of what it is framing and
;;;; is simply designates a rectangular area that can be drawn to. Frames will be used for two main
;;;; purposes: to hold the main text buffer including the scroll bar, and to hold the title buffer
;;;; which also includes the corner showing information such as whether or not the file is modified.
;;;; A buffer frame refers to two frames, one for each of these uses, stored in a cons cell in the
;;;; format (title-buffer-frame . main-text-frame). A title frame refers to a single frame that
;;;; will only be used for displaying a title buffer (e.g. a column header)

(defpackage 9b/frame
  (:use :common-lisp :9b/utils :9b/colors :9b/sys-io)
  (:export make-frame
           fill-frame
           draw-corner
           draw-scroll-bar
           draw-buffer-sep
           draw-frame-sep))

(in-package :9b/frame)

(defvar *scroll-bar-width* 14)
(defvar *fringe-width* 2)

(defun make-frame (x y width height)
  (list (cons :x x) (cons :y y) (cons :width width) (cons :height height)))

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

;; Calling this will overwrite anything else drawn in the frame such as a scroll bar or corner, so
;; the entire frame should be redrawn afterwards in most cases
(defdraw fill-frame (frame color)
  (alist-bind ((:width frame-width) (:height frame-height))
      frame
    (draw-rect frame 0 0 frame-width frame-height color)))

;;; NOTE: All drawing operations expect the frame to be able to fit whatever is being drawn. The
;;; burden of ensuring this falls on the frame management subsystem

(defdraw draw-corner (frame style)
  ;; Draw the corner background
  (draw-rect frame
             0 0
             *scroll-bar-width* (+ (glyph-height) 2) ; Add 2 pixels to account for title padding
             (if (eq style :top-title) *title-buffer-bg-color* *corner-bg-color*))
  ;; Draw the indicator if necessary
  (if (oreq style :modified :unmodified)
      ;; 2 padding pixels will be added on the top, left, and right to ensure the background is
      ;; visible. Only one padding pixel will be added on the bottom because the title separator
      ;; below, which has the same color, will contribute the second pixel thus resulting in an
      ;; even border.
      (draw-rect frame
                 2 2
                 (- *scroll-bar-width* 4) (- (glyph-height) 1)
                 (if (eq style :unmodified)
                     *corner-unmodified-color*
                     *corner-modified-color*))))

(defdraw draw-scroll-bar (frame start% fill%)
  (alist-bind ((:height frame-height))
      frame
    ;; Draw the scroll bar background
    (draw-rect frame 0 0 *scroll-bar-width* frame-height *scroll-bar-bg-color*)

    ;; Find the size and dimensions of the active portion based on start and fill percentages
    (let ((start-y (ceiling (* start% frame-height)))
          (height (ceiling (* fill% frame-height))))
      ;; Draw the active portion of the scroll bar
      (draw-rect frame 0 start-y (1- *scroll-bar-width*) height *scroll-bar-fg-color*))))

;; This separator is used to separate the title from the body in a buffer frame
(defdraw draw-buffer-sep (frame)
  (alist-bind ((:width frame-width))
      frame
    (draw-rect frame 0 (+ (glyph-height) 2) frame-width 1 *title-buffer-sep-color*)))

;; This separator is used to create separation between buffer/title frames
(defdraw draw-sep (x y size vertical-p)
  (move-to x y)
  (if vertical-p
      (progn
        (line-to x (+ y size))
        (line-to (1+ x) (+ y size))
        (line-to (1+ x) y))
      (progn
        (line-to (+ x size) y)
        (line-to (+ x size) (1+ y))
        (line-to x (1+ y))))
  (fill-region))