(in-package :cl)

(defpackage :system-head
  (:use :cl :asdf))

(in-package :system-head)

(defsystem :liutos.cli.head
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the head command."
  :depends-on ()
  :components ((:file "head")))
