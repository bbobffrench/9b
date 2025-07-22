;;;; io.lisp

;;; In the cl-freetype2 system, the element-type of the array created by bitmap-to-array is
;;; 'unsigned-byte. The :data keyword in CLX's render-add-glyph expects an '(unsigned-byte 8) for
;;; an alpha8 picture format. The function has been redefined here to return an array of the
;;; expected element-type.

(defun ft2:bitmap-to-array (bitmap)
  (let ((buffer (ft-bitmap-buffer bitmap))
        (rows (ft-bitmap-rows bitmap))
        (width (ft-bitmap-width bitmap))
        (pitch (ft-bitmap-pitch bitmap))
        (format (ft-bitmap-pixel-mode bitmap)))
    (let ((pixel-fn (ecase format
                      (:mono #'nth-mono-pixel)
                      (:gray #'nth-gray-pixel)
                      (:lcd #'nth-gray-pixel)
                      (:lcd-v #'nth-gray-pixel)))
          (array (make-array (list rows width) :element-type '(unsigned-byte 8))))
      (declare (function pixel-fn))
      (loop for i from 0 below rows
            as ptr = (inc-pointer buffer (* i pitch))
            do (loop for j from 0 below width
                     do (setf (aref array i j) (funcall pixel-fn ptr j)))
            finally (return (values array format))))))

(defpackage 9b/io
  (:use common-lisp)
  (:export font-width
           font-height
           draw-text
           draw-rect
           get-event
           init-window
           destroy-window
           flush))

(in-package 9b/io)

(defvar display nil)
(defvar window nil)
(defvar glyph-set nil)

(defvar font-width nil)
(defvar font-height nil)

(let (glyph-table face)
  (defun register-glyph (char)
    ;; Check that the glyph has not already been loaded
    (if (gethash char glyph-table) (return-from register-glyph))

    (let ((ascent (ceiling (ft2:face-ascender-pixels face)))
          (code (char-code char)))
      (multiple-value-bind (bitmap advance left top) (ft2:default-load-render face code nil)
        (let ((arr (ft2:bitmap-to-array bitmap)))

          ;; In the case of an empty bitmap, create a dummy bitmap to avoid a divide-by-zero error
          (if (every #'zerop (array-dimensions arr))
              (setf arr (make-array '(1 1)
                                    :element-type '(unsigned-byte 8)
                                    :initial-contents '((0)))))

          ;; Load the glyph bitmap into the glyph set
          (xlib:render-add-glyph glyph-set code
                                 :data arr
                                 :x-origin (- left)
                                 :y-origin (- (- ascent top))
                                 :x-advance (ceiling advance)
                                 :y-advance 0))))
    ;; Mark in the glyph table that this glyph has been loaded, so it is not loaded again
    (setf (gethash char glyph-table) t))

  (defun init-font (font-path font-size)
    ;; Check if the supplied face path is valid, and open it if so
    (if (ft2:check-font-file font-path)
        (setf face (ft2:new-face font-path))
        (progn (format t "Font path does not exist or is not a valid format.~%")
               (return-from init-font)))

    ;; Ensure that the font is fixed-width
    (unless (ft2:fixed-face-p face)
      (format t "Variable width fonts are not supported.~%")
      (return-from init-font))

    ;; Fetch the screens DPI and set the font size in pt with respect to this
    (let* ((screen (car (xlib:display-roots display)))
           (screen-width-inches (/ (xlib:screen-width-in-millimeters screen) 25.4))
           (dpi (ceiling (xlib:screen-width screen) screen-width-inches)))
      (ft2:set-char-size face (* font-size 64) 0 dpi dpi))

    (setf font-height (ceiling (nth-value 6 (ft2:face-metrics face)))
          font-width (ceiling (ft2:get-advance face #\X)))

    (let ((format (xlib:find-standard-picture-format display :a8)))
      (setf glyph-set (xlib:render-create-glyph-set format)))

    ;; Ensure glyph table is empty, then register all printable ASCII character glyphs
    (setf glyph-table (make-hash-table :test #'eql))
    (loop for i from 32 to 126 do (register-glyph (code-char i)) return t)))


(let (pixmap dest-picture src-picture
      cur-color) ; The last used color is tracked to avoid duplicate fills
  (defun free-rendering-environment ()
    (when (and pixmap dest-picture src-picture)
      (xlib:free-pixmap pixmap)
      (xlib:render-free-picture dest-picture)
      (xlib:render-free-picture src-picture)
      (setf pixmap nil dest-picture nil src-picture nil cur-color nil)))

  (defun ensure-rendering ()
    (let ((window-width (xlib:drawable-width window))
          (window-height (xlib:drawable-height window)))
      ;; If the size of the window has changed, free resources and prepare for realloc
      (if (and pixmap
               (not (= (xlib:drawable-width pixmap) window-width))
               (not (= (xlib:drawable-height pixmap) window-width)))
          (free-rendering-environment))

      ;; Allocate the rendering environment if this has not already been done
      (if (or (null pixmap) (null dest-picture) (null src-picture))
          (let ((format (xlib:find-standard-picture-format display :argb32)))
            (setf pixmap (xlib:create-pixmap :width window-width
                                             :height window-height
                                             :depth 32
                                             :drawable window)
                  dest-picture (xlib:render-create-picture window)
                  src-picture (xlib:render-create-picture pixmap :format format))))))

  (defun draw-text (rgb str x y)
    ;; If the text doesn't fit inside the window, don't bother drawing it
    (if (> (+ x (* font-width (length str)))
           (xlib:drawable-width window))
        (return-from draw-text))

    ;; Make sure that the environment is ready for rendering
    (ensure-rendering)

    ;; Fill the source picture with the color that the text is to be printed in.
    (unless (equal cur-color rgb)
      (let ((color (append (mapcar (lambda (x) (+ #xff (ash x 8))) rgb) '(#xffff)))
            (width (xlib:drawable-width pixmap))
            (height (xlib:drawable-height pixmap)))
        (xlib:render-fill-rectangle src-picture :over color 0 0 width height)
        (setf cur-color rgb)))

    ;; Make sure that every glyph in the string has been registered
    (map nil #'register-glyph str)

    ;; Render the glyphs to the window using this filled rectangle as the source
    (let ((seq (map 'list #'char-code str)))
      (xlib:render-composite-glyphs dest-picture glyph-set src-picture x y seq))))


(let (gcontext)
  (defun draw-rect (rgb x y width height)
    ;; Allocate a graphics context if this has not yet been done
    (if (null gcontext) (setf gcontext (xlib:create-gcontext :drawable window)))

    ;; Set the foreground color and fill the rectangle
    (let ((color (+ (ash (the (unsigned-byte 8) (car rgb)) 16)
                    (ash (the (unsigned-byte 8) (cadr rgb)) 8)
                    (the (unsigned-byte 8) (caddr rgb)))))
      (setf (xlib:gcontext-foreground gcontext) color)
      (xlib:draw-rectangle window gcontext x y width height t)))

  (defun free-gcontext ()
    (when gcontext
      (xlib:free-gcontext gcontext)
      (setf gcontext nil))))


(let (;; Track dimensions to distinguish resizes from other configure-notify events
      window-width
      window-height

      ;; Track release conditions to generate double-click events
      (release-timestamp 0) (release-x -1) (release-y -1))

  (defun get-event ()
    ;; Assign initial window dimensions on the first call
    (when (or (null window-width) (null window-height))
      (setf window-width (xlib:drawable-width window))
      (setf window-height (xlib:drawable-height window)))
    (xlib:event-case (display)

      (:button-press (code state x y time)
        (let* ((delta-t (- (or time 0) (or release-timestamp 0)))
               (double-click-p (and (= code 1) (< delta-t 500) (= release-x x) (= release-y y)))
               (button (cond (double-click-p :double-click)
                             ((= code 4) :scroll-up)
                             ((= code 5) :scroll-down)
                             (t (intern (format nil "BUTTON-~d" code) "KEYWORD")))))
          (list :button-press
                (cons :button button)
                (cons :state (xlib:make-state-keys state))
                (cons :x x)
                (cons :y y))))

      (:button-release (code x y time)
        (if (= code 1)
            (setf release-timestamp time
                  release-x x
                  release-y y))
        (if (<= code 3)
            (list :button-release
                  (cons :button (intern (format nil "BUTTON-~d" code) "KEYWORD"))
                  (cons :x x)
                  (cons :y y))))

      (:exposure (width height x y)
        (list :exposure
              (cons :width width)
              (cons :height height)
              (cons :x x)
              (cons :y y)))

      (:key-press (code state x y)
        (let* ((sym (xlib:keycode->keysym display code (if (= state 1) 1 0)))
               (key (cond ((= sym #xff1b) :escape)
                          ((= sym #xffff) :delete)
                          ((= sym #xff0d) :return)
                          ((= sym #xff08) :backspace)
                          ((= sym #xff55) :page-up)
                          ((= sym #xff56) :page-down)
                          ((= sym #xff52) :up-arrow)
                          ((= sym #xff54) :down-arrow)
                          ((= sym #xff51) :left-arrow)
                          ((= sym #xff53) :right-arrow)
                          ((= sym #xff09) #\tab)
                          ((and (>= sym 32) (<= sym 236)) (code-char sym)))))
          (if key
              (list :key-press
                    (cons :key key)
                    (cons :state (xlib:make-state-keys state))
                    (cons :x x)
                    (cons :y y))
              (get-event))))

      (:motion-notify (state x y)
        (list :motion
              (cons :state (xlib:make-state-keys state))
              (cons :x x)
              (cons :y y)))

      (:configure-notify (width height)
        (if (and (= width window-width)
                 (= height window-height))
            (get-event)
            (list :resize
                  (cons :width (setf window-width width))
                  (cons :height (setf window-height height)))))

      (:client-message (type data)
        (if (and (eq type :wm_protocols)
                 (= (elt data 0) (xlib:find-atom display :wm_delete_window)))
            '(:destroy-request)
            (get-event))))))


(defun destroy-window ()
  (when (and window display)
    (free-rendering-environment)
    (free-gcontext)
    (xlib:render-free-glyph-set glyph-set)
    (xlib:destroy-window window)
    (xlib:close-display display)
    (setf window nil display nil glyph-set nil)))

(defun init-window (width height font-path font-size)
  (setf display (ignore-errors (xlib:open-default-display)))
  (unless display
    (format t "Could not open display. Is your X server running?~%")
    (return-from init-window))

  ;; Create the window and register for events
  (let ((root (xlib:screen-root (car (xlib:display-roots display))))
        (event-mask '(:button-press :button-release :exposure :key-press :pointer-motion
                      :structure-notify)))
    (setf window (xlib:create-window :parent root
                                     :x 0 :y 0
                                     :width width :height height
                                     :event-mask event-mask)))

  ;; Register window to receive destroy requests before mapping
  (let ((wm-delete-window (xlib:intern-atom display :wm_delete_window)))
    (xlib:change-property window :wm_protocols (list wm-delete-window) :atom 32)
    (xlib:map-window window))

  ;; If font initialization fails, free resources and signal failure
  (if (init-font font-path font-size)
      (progn (xlib:display-finish-output display) t)
      (destroy-window)))

(defun flush ()
  (xlib:display-finish-output display))
