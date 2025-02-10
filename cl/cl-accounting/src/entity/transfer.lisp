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
   (transfer-at
    :documentation "转账发生的日期。"
    :initarg :transfer-at)
   (updated-at))
  (:documentation "转账记录"))

;;; 定义 repo 接口。
(defgeneric create-transfer (repo amount from-account-id to-account-id
                             &key comment transfer-at)
  (:documentation "新增一笔转账。"))

(defgeneric list-by-from-account-id (repo from-account-id)
  (:documentation "列出从给定的 FROM-ACCOUNT-ID 流出的转账记录。"))

(defgeneric list-by-to-account-id (repo to-account-id)
  (:documentation "列出往给定的 TO-ACCOUNT-ID 流入的转账记录。"))

(defgeneric list-transfer (repo)
  (:documentation "列出转账记录。"))
