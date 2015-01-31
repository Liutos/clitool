(in-package :common-lisp)

(defpackage liutos.cli.head
  (:use :common-lisp)
  (:export :head))

(in-package :liutos.cli.head)

;;; head: String|Pathname -> Int -> IO ()
(defun head (path &optional (n 10))
  "Output the first part of a file."
  (with-open-file (s path)
    (handler-case
        (dotimes (i n)
          (let ((line (read-line s)))
            (format t "~A~%" line)))
      (end-of-file ()))))
