;;;; utils.lisp

(defpackage 9b/utils
  (:use common-lisp)
  (:export struct-bind))

(in-package 9b/utils)

(defmacro struct-bind ((&rest slots) var &body body)
  `(let ,(mapcar
           (lambda (slot)
             (let* ((type-str `(symbol-name (type-of ,var)))
                    (slot-str (symbol-name slot))
                    (func-str `(concatenate 'string ,type-str "-" ,slot-str)))
               `(,slot (funcall (intern ,func-str) ,var))))
           slots)
     ,@body))
