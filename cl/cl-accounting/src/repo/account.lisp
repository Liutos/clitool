(in-package cl-user)

(defpackage :cl-accounting.repo
  (:use :cl
        #:cl-accounting.entity)
  (:export #:new-mysql-account-repo))

(in-package :cl-accounting.repo)

(defclass <mysql-account-repo> ()
  ((connection
    :initarg :connection))
  (:documentation "将账户信息存储在 MySQL 中"))

(defmethod create-account ((repo <mysql-account-repo>) name parent-id)
  (with-slots (connection)
      repo
    (dbi:do-sql
      connection
      "INSERT INTO `t_account` (`balance`, `name`, `parent_id`) VALUES (?, ?, ?)"
      (list 0 name parent-id))))

(defmethod get-account ((repo <mysql-account-repo>) id)
  (get-by-unique-key repo "id" id))

(defmethod get-by-name ((repo <mysql-account-repo>) name)
  (get-by-unique-key repo "name" name))

(defun get-by-unique-key (repo key value)
  "基于一个唯一键来查询账户。"
  (with-slots (connection)
      repo
    (let* ((prepared-statement
             (dbi:prepare connection
                          (format nil "SELECT * FROM `t_account` WHERE `~A` = ?" key)))
           (query (dbi:execute prepared-statement (list value)))
           (rows (dbi:fetch-all query)))
      (if rows
          (let ((row (first rows)))
            (make-instance '<account>
                           :balance (getf row :|balance|)
                           :id (getf row :|id|)
                           :name (getf row :|name|)
                           :parent-id (getf row :|parent_id|)))
          nil))))

(defun new-mysql-account-repo (connection)
  (make-instance '<mysql-account-repo>
                 :connection connection))
