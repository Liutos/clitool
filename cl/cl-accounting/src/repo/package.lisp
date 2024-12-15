(in-package cl-user)

(defpackage :cl-accounting.repo
  (:use :cl
        #:cl-accounting.entity)
  (:export #:new-mysql-account-repo
           #:new-mysql-transfer-repo))
