(in-package #:cl-user)

(defpackage #:cl-accounting.app
  (:use #:cl)
  (:export #:<business-error>
           #:<create-account-handler>
           #:<create-transfer-handler>
           #:<delete-account-handler>
           #:get-account-repo
           #:get-transfer-repo
           #:msg-of
           #:run-handler))

(in-package #:cl-accounting.app)

(defgeneric run-handler (handler)
  (:documentation "处理一个请求。"))
