(in-package :common-lisp)

(defpackage :system-grep
  (:use :cl :asdf))

(in-package :system-grep)

(defsystem :liutos.cli.grep
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the grep command."
  :depends-on (:cl-ppcre)
  :components ((:file "grep")))
