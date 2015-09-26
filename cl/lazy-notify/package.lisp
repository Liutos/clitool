(in-package :common-lisp)

(defpackage #:lazy-notify
  (:use :common-lisp
	#:cl-fad
	#:local-time)
  (:export #:callback-helper-pipe
	   #:callback-copy
	   #:callback-info
	   #:mapper-directory
	   #:test-mtime>
	   #:test-true
	   #:with-directory
	   #:with-make))
