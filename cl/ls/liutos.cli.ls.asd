(in-package :common-lisp)

(defpackage :system-ls
  (:use :cl :asdf))

(in-package :system-ls)

(defsystem :liutos.cli.ls
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the ls command."
  :depends-on (:osicat)
  :components ((:file "main")))
