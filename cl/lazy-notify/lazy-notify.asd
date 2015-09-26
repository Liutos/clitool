(in-package :common-lisp)

(defpackage :system-lazy-notify
  (:use :common-lisp :asdf))

(in-package :system-lazy-notify)

(defsystem :lazy-notify
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.0.1"
  :description "Invoke callback if files changed"
  :depends-on (:cl-fad
	       :local-time)
  :components ((:file "main"
		      :depends-on ("package"))
	       (:file "package")))
