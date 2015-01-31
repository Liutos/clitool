(in-package :common-lisp)

(defpackage liutos.cli.cat
  (:use :common-lisp)
  (:export :cat))

(in-package :liutos.cli.cat)

;;; cat-file: String|Pathname -> IO ()
(defun cat-file (file)
  "Print a file on the standard output."
  (with-open-file (s file)
    (loop
       :for line = (read-line s nil)
       :while line
       :do (format t "~A~%" line))))

;;; cat: [String|Pathname] -> IO ()
(defun cat (&rest files)
  "Concatenate files and print on the standard output."
  (dolist (file files)
    (cat-file file)))
