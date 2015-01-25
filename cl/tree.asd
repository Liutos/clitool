(in-package :common-lisp)

(defpackage system-tree
  (:use :cl :asdf))

(in-package :system-tree)

(defsystem :liutos.cli.tree
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the tree command."
  :depends-on (:cl-fad)
  :components
  ((:file "main")))
