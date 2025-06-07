(defpackage 9b/frame
  (:use common-lisp 9b/utils 9b/sys-io)
  (:export ;; colors.lisp
           *fg-color*
           *bg-color*
           *scroll-bar-fg-color*
           *scroll-bar-bg-color*
           *title-bg-color*
           *title-sep-color*
           *corner-bg-color*
           *corner-unmodified-color*
           *corner-modified-color*

           ;; frame.lisp
           make-frame
           fill-frame
           draw-corner
           draw-scroll-bar
           draw-buffer-sep
           draw-frame-sep))