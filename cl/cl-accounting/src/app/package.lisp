(in-package #:cl-user)

(defpackage #:cl-accounting.app
  (:use #:cl)
  (:export #:<business-error>
           #:<create-account-handler>
           #:<create-transfer-handler>
           #:<delete-account-handler>
           #:<list-account-handler>
           #:<list-transfer-handler>
           #:begin-transaction
           #:commit-transaction
           #:get-account-repo
           #:get-transfer-repo
           #:msg-of
           #:rollback-transaction
           #:run-handler
           #:transfer-list-item
           #:transfer-list-item-amount
           #:transfer-list-item-comment
           #:transfer-list-item-created-at
           #:transfer-list-item-from-account-name
           #:transfer-list-item-id
           #:transfer-list-item-to-account-name))

(in-package #:cl-accounting.app)

(defgeneric run-handler (handler)
  (:documentation "处理一个请求。"))
