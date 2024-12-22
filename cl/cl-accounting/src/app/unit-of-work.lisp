(in-package #:cl-accounting.app)

(defgeneric get-account-repo (uow)
  (:documentation "获取一个账户的 repo 对象。"))

(defgeneric get-transfer-repo (uow)
  (:documentation "获取一个转账记录的 repo 对象。"))
