
(in-package :cl)

(defpackage :positional
  (:use :cl)
  (:export
   #:*integer-base*
   #:integer-base
   #:base-parse
   #:base-positional
   #:parse
   #:positional))

(in-package :positional)

(defgeneric base-parse (base input))
(defgeneric base-positional (base number))

(declaim (type simple-string *integer-base*))

(defvar *integer-base*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun integer-base (base)
  (cond ((< base (length *integer-base*))
         (subseq *integer-base* 0 base))
        (t
         (error "Undefined integer base ~D" base))))

#+nil (integer-base 10)
#+nil (integer-base 16)

(defmethod base-parse ((base integer) input)
  (base-parse (integer-base base) input))

(defmethod base-parse ((base string) (input string))
  (let ((base-len (length base))
        (input-len (length input))
        (result 0)
        (i 0))
    (loop
       (unless (< i input-len)
         (return))
       (let* ((char (char input i))
              (pos (position char base :test #'char=)))
         (unless pos
           (error "Invalid char ~S for base ~S." char base))
         (setf result (+ (* result base-len) pos)))
       (incf i))
    result))

(defun parse (input &optional (base 10))
  (base-parse base input))

#+nil (parse "123")
#+nil (parse "DEADBEEF" 16)

(defmethod base-positional ((base string) (number integer))
  (let* ((base-n (length base))
         (out-n (ceiling (log number base-n)))
         (out (make-string out-n :initial-element (char base 0)))
         (i (1- out-n)))
    (loop
       (when (= number 0)
         (return out))
       (when (< i 0)
         (error "bad math"))
       (multiple-value-bind (q r) (floor number base-n)
         (setf number q
               (char out i) (char base r)))
       (decf i))))

#+nil (base-positional (integer-base 16) 100)

(defmethod base-positional ((base integer) number)
  (base-positional (integer-base base) number))

(defun positional (number &optional (base 10))
  (base-positional base number))

#+nil (positional 123)
#+nil (positional 3735928559 16)
