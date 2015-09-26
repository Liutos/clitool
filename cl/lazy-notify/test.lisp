(in-package :common-lisp)

(defpackage #:lazy-notify.test
  (:use :common-lisp
	:lazy-notify)
  (:export #:test-1
	   #:test-2
	   #:test-3))

(in-package :lazy-notify.test)

(defun test-1 ()
  (with-directory
      "./test/src/"
    (lambda (file-src)
      (mapper-directory file-src "./test/dest/"))
    #'test-mtime>
    #'callback-info
    :verbose t))

(defun test-2 ()
  (with-directory
      "./test/src/"
    (lambda (file-src)
      (mapper-directory file-src "./test/dest/"))
    #'test-true
    #'callback-info))

(defun test-3 ()
  (with-directory
      "./test/src/"
    (lambda (file-src)
      (mapper-directory file-src "./test/dest/"))
    #'test-mtime>
    #'callback-copy))
