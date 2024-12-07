(in-package #:cl-user)

(defpackage #:cl-accounting.app
  (:use #:cl)
  (:export #:<business-error>
           #:<create-account-handler>
           #:<delete-account-handler>
           #:msg-of
           #:run-handler))

(in-package #:cl-accounting.app)

(defgeneric run-handler (handler)
  (:documentation "处理一个请求。"))
