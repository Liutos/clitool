(in-package :cl)

(defpackage :system-forth
  (:use :cl :asdf))

(in-package :system-forth)

(defsystem :liutos.cli.forth
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the gforth command."
  :depends-on ()
  :components ((:file "main")))
