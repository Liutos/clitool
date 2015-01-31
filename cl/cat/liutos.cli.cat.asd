(in-package :cl)

(defpackage :system-cat
  (:use :cl :asdf))

(in-package :system-cat)

(defsystem :liutos.cli.cat
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the cat command."
  :depends-on ()
  :components ((:file "main")))
