
(in-package :cl)

(defpackage :positional
  (:use :cl)
  (:export #:parse))

(in-package :positional)

(defgeneric base-parse (base input))
(defgeneric base-positional (base number))

(defvar *integer-base*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun integer-base (base)
  (cond ((< base (length *integer-base*))
         (subseq *integer-base* 0 base))
        (t
         (error "Undefined integer base ~D" base))))

#+test
(integer-base 10)
#+test

(integer-base 16)

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

#+test (parse "123")
#+test (parse "10" 16)

(defmethod base-positional ((base string) (number integer))
  (with-output-to-string 

(defmethod base-positional ((base integer) number)
  (base-positional (integer-base base) number))

(defun positional (number &optional (base 10))
  (base-positional base number))
