;;;; colors.lisp
;;;;
;;;; This file defines the programs default colorscheme

(defpackage 9b/colors
  (:use common-lisp)
  (:export *fg-color*
           *bg-color*
           *scroll-bar-fg-color*
           *scroll-bar-bg-color*
           *title-bg-color*
           *title-sep-color*
           *corner-bg-color*
           *corner-unmodified-color*
           *corner-modified-color*))

(in-package 9b/colors)

(defparameter *fg-color* '(#x00 #x00 #x00))
(defparameter *bg-color* '(#xff #xff #xea))

(defparameter *scroll-bar-fg-color* '(#xff #xff #xea))
(defparameter *scroll-bar-bg-color* '(#x99 #x99 #x4c))

(defparameter *title-bg-color* '(#xea #xff #xff))
(defparameter *title-sep-color* '(#x88 #x88 #xcc))

(defparameter *corner-bg-color* '(#x88 #x88 #xcc))
(defparameter *corner-unmodified-color* '(#xea #xff #xff))
(defparameter *corner-modified-color* '(#x00 #x00 #x99))