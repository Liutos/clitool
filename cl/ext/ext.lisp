(in-package #:lt-cl)

(defun compute-padding-length (str len)
  (check-type str string)
  (check-type len integer)
  (- len (length str)))

(defun make-padding (len ch)
  (check-type len integer)
  (check-type ch character)
  (make-string len :initial-element ch))

(defun string-left-pad (str len ch)
  (check-type str string)
  (check-type len integer)
  (check-type ch character)
  (let* ((len (compute-padding-length str len))
         (padding (make-padding len ch)))
    (concatenate 'string padding str)))

(defun string-right-pad (str len ch)
  (check-type str string)
  (check-type len integer)
  (check-type ch character)
  (let* ((len (compute-padding-length str len))
         (padding (make-padding len ch)))
    (concatenate 'string str padding)))
