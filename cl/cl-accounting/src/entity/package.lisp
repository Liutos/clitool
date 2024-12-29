(in-package :cl-user)

(defpackage :cl-accounting.entity
  (:use :cl)
  (:export #:<account>
           #:<transfer>
           #:account-name
           #:create-account
           #:create-transfer
           #:delete-account
           #:get-account
           #:get-by-name
           #:list-by-parent-id
           #:list-transfer
           #:lock-account-by-id
           #:transfer-amount
           #:transfer-comment
           #:transfer-created-at
           #:transfer-from-account-id
           #:transfer-id
           #:transfer-to-account-id))
