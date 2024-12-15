(in-package :cl-user)

(defpackage :cl-accounting.entity
  (:use :cl)
  (:export #:<account>
           #:<transfer>
           #:create-account
           #:create-transfer
           #:delete-account
           #:get-account
           #:get-by-name
           #:list-by-parent-id))
