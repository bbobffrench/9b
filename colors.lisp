;;;; colors.lisp
;;;;
;;;; This file defines the programs default colorscheme

(defpackage 9b/colors
  (:use :common-lisp)
  (:export *fg-color*
           *bg-color*
           *scroll-bar-fg-color*
           *scroll-bar-bg-color*
           *mini-buffer-bg-color*
           *mini-buffer-sep-color*
           *corner-bg-color*
           *corner-fg-color*
           *corner-border-color*))

(in-package :9b/colors)

(defvar *fg-color* '(#x00 #x00 #x00))
(defvar *bg-color* '(#xff #xff #xea))

(defvar *scroll-bar-fg-color* '(#xff #xff #xea))
(defvar *scroll-bar-bg-color* '(#x99 #x99 #x4c))

(defvar *mini-buffer-bg-color* '(#xea #xff #xff))
(defvar *mini-buffer-sep-color* '(#x88 #x88 #xcc))

(defvar *corner-bg-color* '(#xea #xff #xff))
(defvar *corner-fg-color* '(#x00 #x00 #x99))
(defvar *corner-border-color* '(#x88 #x88 #xcc))