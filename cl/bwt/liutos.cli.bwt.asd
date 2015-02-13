(in-package :cl)

(defpackage :system-bwt
  (:use :cl :asdf))

(in-package :system-bwt)

(defsystem :liutos.cli.bwt
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Burrows–Wheeler transform. For details, see http://en.wikipedia.org/wiki/Burrows–Wheeler_transform."
  :depends-on ()
  :components ((:file "tail")))
