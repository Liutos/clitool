(in-package :common-lisp)

(defpackage :system-tar
  (:use :cl :asdf))

(in-package :system-tar)

(defsystem :liutos.cli.tar
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the tar command."
  :depends-on
  (:flexi-streams
   :osicat)
  :components ((:file "main")))
