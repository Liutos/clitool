(in-package #:common-lisp)

(defpackage #:gnu-find-test
  (:use #:common-lisp))

(in-package #:gnu-find-test)

(prove:plan 9)

(prove:is (gnu-find::stringify-test "newerXY") "newerXY")
(prove:is (gnu-find::stringify-test 'links) "links")

(prove:is (gnu-find::stringify-expression '(links 1)) " -links 1")
(prove:is (gnu-find::stringify-expression '(name "*lisp")) " -name \"*lisp\"")

(prove:is (gnu-find::stringify-and-expressions '((links 1) (name "*lisp"))) " -links 1 -a -name \"*lisp\"")
(prove:is (gnu-find::stringify-not-expressions '((name "*lisp"))) " -not -name \"*lisp\"")
(prove:is (gnu-find::stringify-or-expressions '((links 1) (name "*lisp"))) " -links 1 -o -name \"*lisp\"")

(prove:is (gnu-find::stringify-expressions '((links 1))) " -links 1")
(prove:is (gnu-find::stringify-expressions '((links 1) (name "*lisp"))) " -links 1 -name \"*lisp\"")
(prove:is (gnu-find::stringify-expressions '((and (links 1) (name "*lisp")))) " -links 1 -a -name \"*lisp\"")

(prove:finalize)
