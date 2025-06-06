;;;; colors.lisp
;;;;
;;;; This file defines the programs default colorscheme

(defpackage 9b/colors
  (:use :common-lisp)
  (:export *fg-color*
           *bg-color*
           *scroll-bar-fg-color*
           *scroll-bar-bg-color*
           *title-buffer-bg-color*
           *title-buffer-sep-color*
           *corner-bg-color*
           *corner-unmodified-color*
           *corner-modified-color*))

(in-package :9b/colors)

(defvar *fg-color* '(#x00 #x00 #x00))
(defvar *bg-color* '(#xff #xff #xea))

(defvar *scroll-bar-fg-color* '(#xff #xff #xea))
(defvar *scroll-bar-bg-color* '(#x99 #x99 #x4c))

(defvar *title-buffer-bg-color* '(#xea #xff #xff))
(defvar *title-buffer-sep-color* '(#x88 #x88 #xcc))

(defvar *corner-bg-color* '(#x88 #x88 #xcc))
(defvar *corner-unmodified-color* '(#xea #xff #xff))
(defvar *corner-modified-color* '(#x00 #x00 #x99))