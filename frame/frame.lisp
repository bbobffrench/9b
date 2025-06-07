;;;; frame.lisp
;;;;
;;;; This file implements the concept of a frame. A frame has no knowledge of what it is framing and
;;;; is simply designates a rectangular area that can be drawn to. Frames will be used for two main
;;;; purposes: to hold the main text buffer including the scroll bar, and to hold the title buffer
;;;; which also includes the corner showing information such as whether or not the file is modified.
;;;; A buffer frame refers to two frames, one for each of these uses, stored in a cons cell in the
;;;; format (title-frame . body-frame). A title frame refers to a single frame that will only be used
;;;; for displaying a title buffer (e.g. a column header)

(in-package 9b/frame)

(defparameter *scroll-bar-width* 13)
(defparameter *fringe-width* 4)

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
             *scroll-bar-width* (glyph-height)
             (if (eq style :top-title) *title-bg-color* *corner-bg-color*))
  ;; Draw the indicator if necessary
  (if (oreq style :modified :unmodified)
      ;; 2 padding pixels will be added on the top, left, and right to ensure the background is
      ;; visible. Only one padding pixel will be added on the bottom because the title separator
      ;; below, which has the same color, will contribute the second pixel thus resulting in an
      ;; even border.
      (draw-rect frame
                 2 2
                 (- *scroll-bar-width* 4) (- (glyph-height) 3)
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
    (draw-rect frame 0 (glyph-height) frame-width 1 *title-sep-color*)))

(defdraw draw-buffer-frame (buffer-frame corner-style start% fill%)
  (let ((title (car buffer-frame)) (body (cdr buffer-frame)))
    (fill-frame title *title-bg-color*)
    (draw-corner title corner-style)
    (draw-buffer-sep title)
    (fill-frame body *bg-color*)
    (draw-scroll-bar body start% fill%)))

(defdraw draw-title-frame (title-frame corner-style)
  (fill-frame title-frame *title-bg-color*)
  (draw-corner title-frame corner-style))

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

(defun col-to-x (col)
  (+ *scroll-bar-width* *fringe-width* (* (glyph-width) col)))

(defun row-to-y (row)
  (* (glyph-height) row))

(defun frame-cols (frame)
  (alist-bind ((:width frame-width))
      frame
    (floor frame-width (glyph-width))))

(defun frame-rows (frame)
  (alist-bind ((:height frame-height))
      frame
    (floor frame-height (glyph-height))))

(defdraw fill-row (frame row color)
  (alist-bind ((:width frame-width))
      frame
    (draw-rect frame
               (+ *scroll-bar-width* *fringe-width*) (row-to-y row)
               (- frame-width *scroll-bar-width* *fringe-width*) (glyph-height)
               color)))

(defdraw fill-row-after-col (frame row col color)
  (alist-bind ((:width frame-width))
      frame
    (draw-rect frame
               (col-to-x col) (row-to-y row)
               (- frame-width (col-to-x col)) (glyph-height)
               color)))

(defdraw fill-row-section (frame row start-col end-col color)
  (draw-rect frame
             (col-to-x start-col) (row-to-y row)
             (- (col-to-x end-col) (col-to-x start-col)) (glyph-height)
             color))

;; This will be used for clearing buffer text, as well as highlighting a selection
(defdraw fill-section (frame start-row start-col end-row end-col color)
  (cond ((= start-row end-row)
         ;; For the last row being filled, fill only the desired section
         (fill-row-section frame start-row start-col end-col color))
        ((zerop start-col)
         ;; For the middle rows, fill from the start of the line to the edge of the frame
         (progn
           (fill-row frame start-row color)
           (fill-section frame (1+ start-row) 0 end-row end-col color)))
        ;; For the first row, clear from the starting column to the edge of the frame
        (t (progn
             (fill-row-after-col frame start-row start-col color)
             (fill-section frame (1+ start-row) 0 end-row end-col color)))))

;; This is necessary to clean up cursor cap fragments left in the fringe and should be called
;; whenever the cursor is moved from column 0
(defdraw clear-fringe (frame title-p)
  (alist-bind ((:height frame-height))
      frame
    (draw-rect frame
               *scroll-bar-width* 0 *fringe-width* frame-height
               (if title-p *title-bg-color* *bg-color*))))

(defdraw draw-text (frame str row col)
  (set-color *fg-color*)
  (alist-bind ((:x start-x) (:y start-y))
      frame
    (move-to (+ start-x (col-to-x col))
             (+ start-y (glyph-ascent) (row-to-y row)))
    (print-string str)))

(defdraw draw-cursor (frame row col)
  (let ((start-x (col-to-x col)) (start-y (row-to-y row)))
    ;; Draw central bar
    (draw-rect frame start-x start-y 1 (glyph-height) *fg-color*)

    ;; Blank out areas to the left and right
    (draw-rect frame (1- start-x) start-y 1 (glyph-height) *bg-color*)
    (draw-rect frame (1+ start-x) start-y 1 (glyph-height) *bg-color*)

    ;; Add top and bottom caps
    (draw-rect frame (1- start-x) start-y 3 3 *fg-color*)
    (draw-rect frame (1- start-x) (+ start-y (- (glyph-height) 3)) 3 3 *fg-color*)))