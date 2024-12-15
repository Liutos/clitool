(in-package #:cl-accounting.entity)

(defclass <transfer> ()
  ((created-at)
   (amount
    :initarg :amount)
   (comment
    :initarg :comment)
   (from-account-id
    :initarg :from-account-id)
   (id
    :initarg :id)
   (to-account-id
    :initarg :to-account-id)
   (updated-at))
  (:documentation "转账记录"))

;;; 定义 repo 接口。
(defgeneric create-transfer (repo amount from-account-id to-account-id)
  (:documentation "新增一笔转账。"))
