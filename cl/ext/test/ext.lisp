(in-package #:lt-cl-test)

(plan 4)

(is (lt-cl::compute-padding-length "foobar" 10) 4)
(is (lt-cl::make-padding 3 #\a) "aaa")

(is (lt-cl:string-left-pad "foobar" 10 #\a) "aaaafoobar")
(is (lt-cl:string-right-pad "foobar" 10 #\a) "foobaraaaa")

(finalize)
