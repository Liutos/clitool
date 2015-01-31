(in-package :cl)

(defpackage :system-cp
  (:use :cl :asdf))

(in-package :system-cp)

(defsystem :liutos.cli.cp
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the cp command."
  :depends-on ()
  :components ((:file "cp")))
