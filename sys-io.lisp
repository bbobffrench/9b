;;;; Defines an interface to X11 and Cairo through `sys-io.c' for handling graphics and user input

(ql:quickload "cffi")

(cffi:define-foreign-library sys-io (t "sys-io.so"))
(cffi:use-foreign-library sys-io)

;;; Window creation, deletion, and querying
;;;
(cffi:defcfun "create_window" :pointer
  (width :uint16)
  (height :uint16)
  (font :string)
  (font-size :uint8))

(defparameter *window* (create-window 800 800 "Iosevka" 10))

(cffi:defcfun ("destroy_window" destroy-window%) :void
  (window :pointer))

(defun destroy-window ()
  (destroy-window% *window*))

(cffi:defcfun ("window_width" window-width%) :uint16
  (window :pointer))

(defun window-width ()
  (window-width% *window*))

(cffi:defcfun ("window_height" window-height%) :uint16
  (window :pointer))

(defun window-height ()
  (window-height% *window*))

(cffi:defcfun ("glyph_width" glyph-width%) :uint8
  (window :pointer))

(defun glyph-width ()
  (glyph-width% *window*))

(cffi:defcfun ("glyph_height" glyph-height%) :uint8
  (window :pointer))

(defun glyph-height ()
  (glyph-height% *window*))

;;; Drawing of text and primitive graphics to the window
;;;
(cffi:defcfun ("display_window" display-window%) :void
  (window :pointer))

(defun display-window ()
  (display-window% *window*))

(cffi:defcfun ("set_color" set-color%) :void
  (window :pointer)
  (r :uint8)
  (g :uint8)
  (b :uint8))

(defun set-color (red green blue)
  (set-color% *window* red green blue))

(cffi:defcfun ("move_to" move-to%) :void
  (window :pointer)
  (x :uint16)
  (y :uint16))

(defun move-to (x y)
  (move-to% *window* x y))

(cffi:defcfun ("line_to" line-to%) :void
  (window :pointer)
  (x :uint16)
  (y :uint16))

(defun line-to (x y)
  (line-to% *window* x y))

(cffi:defcfun ("draw" draw%) :void
  (window :pointer))

(defun draw ()
  (draw% *window*))

(cffi:defcfun ("fill_region" fill-region%) :void
  (window :pointer))

(defun fill-region ()
  (fill-region% *window*))

(cffi:defcfun ("clear_window" clear-window%) :void
  (window :pointer))

(defun clear-window ()
  (clear-window% *window*))

(cffi:defcfun ("print_string" print-string%) :void
  (window :pointer)
  (str :string))

(defun print-string (str)
  (print-string% *window* str))

;;; Handling of user input and window state events
;;;
(cffi:defcenum event-types
  :none
  :expose
  :button-left-press
  :button-middle-press
  :button-right-press
  :scroll-down
  :scroll-up
  :key-press
  :motion
  :resize)

(cffi:defcenum event-states
  (:button-left 1)
  (:button-middle 2)
  (:button-right 4)
  (:control 8))

(cffi:defcenum special-keys
  (:backspace #x1ff)
  (:left #x2ff)
  (:right #x3ff)
  (:up #x4ff)
  (:down #x5ff))

(cffi:defcfun ("get_event" get-event%) :uint64
  (window :pointer))

(defun decode-state (state)
  (if (zerop state)
      nil
      ;; Isolate the least significant set bit
      (let ((isolated-lssb (logand state (1+ (lognot state)))))
        (cons (cffi:foreign-enum-keyword 'event-states isolated-lssb)
              (decode-state (logand state (lognot isolated-lssb)))))))

(defun interpret-signed-16 (unsigned)
  (logior unsigned (- (mask-field (byte 1 15) unsigned))))

(defun get-event ()
  (let* ((event-mask (get-event% *window*))
         (type (cffi:foreign-enum-keyword 'event-types (logand #xff event-mask))))
    (cond ((or (eq type :button-left-press)
               (eq type :button-middle-press)
               (eq type :button-right-press)
               (eq type :motion))
           ;; For button presses and motion, extract state and pointer coordinates
           (let ((state (decode-state (ash (logand #xff00 event-mask) -8)))
                 (x (interpret-signed-16 (ash (logand #xffff0000 event-mask) -16)))
                 (y (interpret-signed-16 (ash (logand #xffff00000000 event-mask) -32))))
             (list type state (list x y))))
          ((eq type :key-press)
           ;; For key press events, extract state and the pressed key
           (let ((state (decode-state (ash (logand #xff00 event-mask) -8)))
                 (key (ash (logand #xffff0000 event-mask) -16)))
             (if (> key #xff)
                 (list type state (cffi:foreign-enum-keyword 'special-keys key))
                 (list type state (code-char key)))))
          ;; Otherwise, just return the event type
          (t (list type)))))

(defun event-test ()
  (let ((event (get-event)))
    (cond ((eq event :none) (event-test))
          (t
           (progn
             (print event)
             (finish-output)
             (event-test))))))

(event-test)