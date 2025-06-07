(ql:quickload "cffi")

(load "utils.lisp")

(load "sys-io/sys-io.lisp")

(load "frame/package.lisp")
(load "frame/colors.lisp")
(load "frame/frame.lisp")

(in-package 9b/frame)

(create-window 800 800 "Iosevka" 10)
(let ((title (make-frame 0 0 800 (glyph-height)))
      (body (make-frame 0 (1+ (glyph-height)) 800 (- 800 (1+ (glyph-height))))))
  (draw-buffer-frame (cons title body) :unmodified 0.2 0.6)
  (draw-text title "New Cut Paste Snarf" 0 0)
  (draw-text body "The () quick brown fox jumps over the lazy dog." 0 0)
  (draw-text body "There was a great big moose. He liked to drink a lot of juice." 1 0)
  (draw-cursor body 1 1 t)
  (sleep 5)
  (destroy-window))