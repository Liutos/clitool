(in-package :cl)

(defpackage :system-md5sum
  (:use :cl :asdf))

(in-package :system-md5sum)

(defsystem :liutos.cli.md5sum
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the md5sum command."
  :depends-on ()
  :components ((:file "main")))
