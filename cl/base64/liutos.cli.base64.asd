(in-package :cl)

(defpackage :system-base64
  (:use :cl :asdf))

(in-package :system-base64)

(defsystem :liutos.cli.base64
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the base64 command."
  :depends-on ()
  :components ((:file "main")))
