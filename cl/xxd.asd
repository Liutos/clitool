(in-package :common-lisp)

(defpackage :system-xxd
  (:use :cl :asdf))

(in-package :system-xxd)

(defsystem :liutos.cli.xxd
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the Linux xxd command."
  :depends-on ()
  :components ((:file "xxd")))
