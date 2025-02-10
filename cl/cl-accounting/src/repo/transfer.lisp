(in-package #:cl-accounting.repo)

(defclass <mysql-transfer-repo> ()
  ((connection
    :initarg :connection))
  (:documentation "将转账记录存储到 MySQL 中"))

(defmethod create-transfer ((repo <mysql-transfer-repo>) amount from-account-id to-account-id
                            &key (comment "")
                              transfer-at)
  (with-slots (connection) repo
    (dbi:do-sql
      connection
      "INSERT INTO `t_transfer` (`amount`, `comment`, `from_account_id`, `to_account_id`, `transfer_at`) VALUES (?, ?, ?, ?, ?)"
      (list amount comment from-account-id to-account-id (convert-transfer-at transfer-at)))))

(defmethod list-by-from-account-id ((repo <mysql-transfer-repo>) from-account-id)
  (with-slots (connection) repo
    (let* ((prepared-statement
             (dbi:prepare connection
                          (format nil "SELECT * FROM `t_transfer` WHERE `from_account_id` = ?")))
           (query (dbi:execute prepared-statement (list from-account-id)))
           (rows (dbi:fetch-all query)))
      (mapcar #'convert-row-to-transfer rows))))

(defmethod list-by-to-account-id ((repo <mysql-transfer-repo>) to-account-id)
  (with-slots (connection) repo
    (let* ((prepared-statement
             (dbi:prepare connection
                          (format nil "SELECT * FROM `t_transfer` WHERE `to_account_id` = ?")))
           (query (dbi:execute prepared-statement (list to-account-id)))
           (rows (dbi:fetch-all query)))
      (mapcar #'convert-row-to-transfer rows))))

(defmethod list-transfer ((repo <mysql-transfer-repo>))
  (with-slots (connection) repo
    (let* ((prepared-statement
             (dbi:prepare connection
                          (format nil "SELECT * FROM `t_transfer`")))
           (query (dbi:execute prepared-statement))
           (rows (dbi:fetch-all query)))
      (mapcar #'convert-row-to-transfer rows))))

(defun convert-row-to-transfer (row)
  "将表 t_transfer 的一行转换为转账实体对象。"
  (make-instance '<transfer>
                 :amount (getf row :|amount|)
                 :comment (getf row :|comment|)
                 :created-at (getf row :|created_at|)
                 :from-account-id (getf row :|from_account_id|)
                 :id (getf row :|id|)
                 :to-account-id (getf row :|to_account_id|)))

(defun convert-transfer-at (transfer-at)
  "将 TRANSFER-AT 转换为可以写入到数据库中的格式。"
  (local-time:format-timestring nil transfer-at :format '(:year "-" (:month 2) "-" (:day 2))))

(defun new-mysql-transfer-repo (connection)
  (make-instance '<mysql-transfer-repo>
                 :connection connection))
