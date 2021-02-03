
(in-package :positional)

(defgeneric base-parse (base input))
(defgeneric base-positional (base number))

(declaim (type simple-string *integer-base*))

(defvar *integer-base*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defparameter *integer-base-cache*
  (cons *integer-base*
        (make-array `(,(length *integer-base*))
                    :initial-element nil)))

(defun update-integer-base-cache (ib)
  (declare (type string ib))
  (let ((ibc *integer-base-cache*)
        (ib-n (length ib)))
    (setf (first ibc) ib)
    (if (= ib-n (length (the simple-vector (rest ibc))))
        (let ((i 0))
          (loop (unless (< i ib-n) (return))
             (setf (svref (rest ibc) i) nil)
             (incf i)))
        (setf (rest ibc)
              (make-array `(,ib-n) :initial-element nil)))))

(defun integer-base (base)
  (declare (type fixnum base))
  (let* ((ib *integer-base*)
         (ibc *integer-base-cache*))
    (unless (<= 2 base (length ib))
      (error "Undefined integer base ~D" base))
    (or (cond ((= base (length ib)) ib)
              ((eq ib (first ibc))
               (svref (rest ibc) (1- base)))
              (t (update-integer-base-cache ib) nil))
        (setf (svref (rest ibc) (1- base))
              (let ((b (subseq ib 0 base)))
                (format t "~&IB ~S b ~S~%" ib b)
                b)))))

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
  (cond ((< number 0)
         (concatenate 'string "-" (base-positional base (- number))))
        ((= number 0)
         (make-string 1 :initial-element (char base 0)))
        ((= 1 (length base))
         (make-string number :initial-element (char base 0)))
        (t
         (let* ((base-n (length base))
                (out-n (1+ (floor (log number base-n))))
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
              (decf i))))))

#+nil (base-positional (integer-base 16) 100)

(defmethod base-positional ((base integer) number)
  (base-positional (integer-base base) number))

(defun positional (number &optional (base 10))
  (base-positional base number))

#+nil (positional 123)
#+nil (positional 3735928559 16)
#+nil (positional 0 9)
#+nil (positional 9 9)
#+nil (positional 99 9)
#+nil (positional 9000 9)

(defmethod base-quote ((base string) (number integer))
  (cond ((< number 0)
         (concatenate 'string "-" (base-quote base (- number))))
        ((= 1 (length base))
         (error "cannot quote base 1"))
        (t
         (let* ((base-n (length base))
                (positional (base-positional base number))
                (prefix (make-string (length positional)
                                     :initial-element (char base (1- base-n))))
                (separator (make-string 1 :initial-element (char base 0))))
           (concatenate 'string prefix separator positional)))))

(defmethod base-quote ((base integer) number)
  (base-quote (integer-base base) number))

(defun quoted (number &optional (base 10))
  (base-quote base number))

#+nil (quoted 0)
#+nil (quoted 123)
#+nil (quoted 0 2)
#+nil (quoted 1 2)
#+nil (quoted 9 9)
#+nil (quoted 99 9)
#+nil (quoted 9000 9)
#+nil (quoted 9 2)
#+nil (quoted 99 2)
#+nil (quoted 9000 2)
