(in-package :cl-user)

(defpackage #:cl-accounting.infra
  (:use #:cl)
  (:export #:get-connection
           #:init
           #:with-connection))

(in-package #:cl-accounting.infra)

(defvar *connection* nil
  "MySQL 数据库连接。")

(defun get-connection ()
  (unless *connection*
    (init))

  *connection*)

(defun init ()
  "初始化数据库连接。"
  (setf *connection*
        (dbi:connect :mysql
                     :host (uiop:getenv "MYSQL_HOST")
                     :database-name (uiop:getenv "MYSQL_DATABASE_NAME")
                     :username (uiop:getenv "MYSQL_USERNAME")
                     :password (uiop:getenv "MYSQL_PASSWORD"))))

(defmacro with-connection ((connection) &body body)
  "封装一下 dbi:with-connection。"
  `(dbi:with-connection (,connection
                         :mysql
                         :host (uiop:getenv "MYSQL_HOST")
                         :database-name (uiop:getenv "MYSQL_DATABASE_NAME")
                         :username (uiop:getenv "MYSQL_USERNAME")
                         :password (uiop:getenv "MYSQL_PASSWORD"))
     ,@body))
