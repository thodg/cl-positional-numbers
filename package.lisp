
(in-package :common-lisp-user)

(defpackage :positional
  (:use :cl)
  (:export
   #:*integer-base*
   #:integer-base
   #:base-parse
   #:base-positional
   #:parse
   #:positional))
