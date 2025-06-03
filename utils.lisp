;;;; utils.lisp
;;;;
;;;; This file provides some generic utilities that are not specific to any one file

(defpackage 9b/utils
  (:use :common-lisp)
  (:export oreq))

(in-package :9b/utils)

(defun oreq (item &rest comparisons)
  (cond ((null comparisons) nil)
        ((eq item (car comparisons)) t)
        (t (apply #'oreq `(,item ,@(cdr comparisons))))))