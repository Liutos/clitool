(in-package #:cl-accounting.app)

(defgeneric begin-transaction (uow)
  (:documentation "开启数据库事务。"))

(defgeneric commit-transaction (uow)
  (:documentation "提交数据库事务。"))

(defgeneric get-account-repo (uow)
  (:documentation "获取一个账户的 repo 对象。"))

(defgeneric get-transfer-repo (uow)
  (:documentation "获取一个转账记录的 repo 对象。"))

(defgeneric rollback-transaction (uow)
  (:documentation "回滚数据库事务。"))
