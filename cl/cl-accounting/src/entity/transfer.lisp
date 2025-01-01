(in-package #:cl-accounting.entity)

(defclass <transfer> ()
  ((amount
    :initarg :amount
    :reader transfer-amount)
   (comment
    :initarg :comment
    :reader transfer-comment)
   (created-at
    :initarg :created-at
    :reader transfer-created-at)
   (from-account-id
    :initarg :from-account-id
    :reader transfer-from-account-id)
   (id
    :initarg :id
    :reader transfer-id)
   (to-account-id
    :initarg :to-account-id
    :reader transfer-to-account-id)
   (updated-at))
  (:documentation "转账记录"))

;;; 定义 repo 接口。
(defgeneric create-transfer (repo amount from-account-id to-account-id
                             &key comment)
  (:documentation "新增一笔转账。"))

(defgeneric list-transfer (repo)
  (:documentation "列出转账记录。"))
