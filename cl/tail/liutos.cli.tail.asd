(in-package :cl)

(defpackage :system-tail
  (:use :cl :asdf))

(in-package :system-tail)

(defsystem :liutos.cli.tail
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the tail command."
  :depends-on ()
  :components ((:file "tail")))
