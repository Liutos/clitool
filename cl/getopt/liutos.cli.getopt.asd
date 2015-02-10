(in-package :cl-user)

(defpackage :system-getopt
  (:use :cl :asdf))

(in-package :system-getopt)

(defsystem :liutos.cli.getopt
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the getopt command."
  :depends-on ()
  :components ((:file "main")))
