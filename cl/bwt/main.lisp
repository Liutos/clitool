(in-package :common-lisp)

(defpackage liutos.cli.bwt
  (:use :common-lisp)
  (:export :bwt))

(in-package :liutos.cli.bwt)

(defun string-rotate (str n)
  (concatenate 'string (subseq str n) (subseq str 0 n)))

(defun mkmatrix (str)
  (let ((result '()))
    (dotimes (i (length str) (nreverse result))
      (push (string-rotate str i) result))))

(defun bwt (str)
  (let ((mtx (sort (mkmatrix str) #'string<))
        (res '()))
    (dolist (s mtx (coerce (nreverse res) 'string))
      (push (char s (1- (length s))) res))))
