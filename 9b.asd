;;;; 9b.asd

(defpackage 9b-system (:use common-lisp asdf))
(in-package 9b-system)

(defsystem "9b"
  :author "Bob French <bobfrenc@buffalo.edu>"
  :depends-on ("clx" "cl-freetype2")

  :components
  ((:file "ft2-patch")
   (:file "io" :depends-on ("ft2-patch"))
   (:file "9b" :depends-on ("io"))))
