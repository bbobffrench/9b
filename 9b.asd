;;;; 9b.asd

(defpackage 9b-system (:use common-lisp asdf))
(in-package 9b-system)

(defsystem "9b"
  :author "Bob French <bobfrenc@buffalo.edu>"
  :depends-on ("clx" "cl-freetype2")

  :components
  ((:file "utils")
   (:file "buffer" :depends-on ("utils"))
   (:file "io")
   (:file "9b" :depends-on ("io"))))
