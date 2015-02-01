(in-package :common-lisp)

(defpackage system-wc
  (:use :cl :asdf))

(in-package :system-wc)

(defsystem :liutos.cli.wc
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Simulate the wc command."
  :depends-on
  (:flexi-streams
   :split-sequence)
  :components
  ((:file "main")))
