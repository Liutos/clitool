(in-package :common-lisp)

(defpackage liutos.cli.grep
  (:use :common-lisp
        :cl-ppcre)
  (:export :grep))

(in-package :liutos.cli.grep)

;;; matchp: String -> String -> Bool
(defun matchp (pat line &key (ignore-case nil) (invert-match nil) &allow-other-keys)
  "Check whether a line matches a pattern."
  (let ((res (if ignore-case
                 (scan (string-upcase pat) (string-upcase line))
                 (scan pat line))))
    (if invert-match
        (not res)
        res)))

;;; line-print: String -> String -> Int -> Bool -> IO ()
(defun line-print (line file lino &key (numberp nil) &allow-other-keys)
  "Print matching line"
  (when numberp
    (format t "~A:~D:" file lino))
  (format t "~A~%" line))

;;; grep: String -> String|Pathname -> IO ()
;;; 实际上，对于允许关键字参数的函数，函数声明没办法很好地定义
(defun grep (pat file &rest args &key (numberp nil) (ignore-case nil) (invert-match nil))
  "Print lines matching a pattern."
  (declare (ignorable numberp ignore-case invert-match))
  (with-open-file (s file)
    (let ((lino 1))
      (loop
         :for line = (read-line s nil)
         :while line
         :if (apply #'matchp pat line args)
         :do (apply #'line-print line file lino args)
         :do (incf lino)))))
