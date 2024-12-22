(in-package #:cl-accounting.repo)

(defclass <mysql-unit-of-work> ()
  ((connection
    :initarg :connection))
  (:documentation "Unit-Of-Work 模式的实现者。"))

(defmethod begin-transaction ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (dbi:do-sql
      connection
      "BEGIN")
    (format t "开启数据库事务~%")))

(defmethod commit-transaction ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (dbi:do-sql
      connection
      "COMMIT")
    (format t "提交数据库事务~%")))

(defmethod get-account-repo ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (new-mysql-account-repo connection)))

(defmethod get-transfer-repo ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (new-mysql-transfer-repo connection)))

(defmethod rollback-transaction ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (dbi:do-sql
      connection
      "ROLLBACK")
    (format t "回滚数据库事务~%")))

(defun new-mysql-unit-of-work (connection)
  (make-instance '<mysql-unit-of-work>
                 :connection connection))
