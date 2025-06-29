;;;; utils.lisp
;;;;
;;;; This file provides some generic utilities that are not specific to any one file

(defpackage 9b/utils
  (:use :common-lisp)
  (:export oreq
           alist-bind
           alist-set))

(in-package :9b/utils)

(defun oreq (item &rest comparisons)
  (cond ((null comparisons) nil)
        ((eq item (car comparisons)) t)
        (t (apply #'oreq `(,item ,@(cdr comparisons))))))

(defmacro alist-bind (slots alist &body body)
  `(let ,(mapcar (lambda (slot) `(,(cadr slot) (cdr (assoc ,(car slot) ,alist))))
                 slots)
     ,@body))

(defun alist-set (alist item value)
  (setf (cdr (assoc item alist)) value))