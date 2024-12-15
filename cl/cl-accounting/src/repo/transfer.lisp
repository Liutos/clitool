(in-package #:cl-accounting.repo)

(defclass <mysql-transfer-repo> ()
  ((connection
    :initarg :connection))
  (:documentation "将转账记录存储到 MySQL 中"))

(defmethod create-transfer ((repo <mysql-transfer-repo>) amount from-account-id to-account-id)
  (with-slots (connection) repo
    (dbi:do-sql
      connection
      "INSERT INTO `t_transfer` (`amount`, `from_account_id`, `to_account_id`) VALUES (?, ?, ?)"
      (list amount from-account-id to-account-id))))

(defun new-mysql-transfer-repo (connection)
  (make-instance '<mysql-transfer-repo>
                 :connection connection))
