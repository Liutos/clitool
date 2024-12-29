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

(defmethod list-transfer ((repo <mysql-transfer-repo>))
  (with-slots (connection) repo
    (let* ((prepared-statement
             (dbi:prepare connection
                          (format nil "SELECT * FROM `t_transfer`")))
           (query (dbi:execute prepared-statement))
           (rows (dbi:fetch-all query)))
      (mapcar #'(lambda (row)
                  (make-instance '<transfer>
                                 :amount (getf row :|amount|)
                                 :comment (getf row :|comment|)
                                 :created-at (getf row :|created_at|)
                                 :from-account-id (getf row :|from_account_id|)
                                 :id (getf row :|id|)
                                 :to-account-id (getf row :|to_account_id|)))
              rows))))

(defun new-mysql-transfer-repo (connection)
  (make-instance '<mysql-transfer-repo>
                 :connection connection))
