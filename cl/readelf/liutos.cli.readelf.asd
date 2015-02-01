(in-package :common-lisp)

(defpackage :system-readelf
  (:use :cl :asdf))

(in-package :system-readelf)

(defsystem :liutos.cli.readelf
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the readelf command."
  :depends-on ()
  :components ((:file "main")))
