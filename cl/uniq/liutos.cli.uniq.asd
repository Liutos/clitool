(in-package :cl-user)

(defpackage :system-uniq
  (:use :cl :asdf))

(in-package :system-uniq)

(defsystem :liutos.cli.uniq
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the uniq command."
  :depends-on ()
  :components ((:file "main")))
