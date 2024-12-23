(in-package :cl-accounting.entity)

(defclass <account> ()
  ((created-at)
   (balance
    :initarg :balance)
   (id
    :initarg :id)
   (name
    :initarg :name)
   (parent-id
    :initarg :parent-id)
   (updated-at))
  (:documentation "账户"))

(defmethod print-object ((object <account>) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (balance id name parent-id)
        object
      (format stream "balance: ~A; id: ~A; name: ~A; parent-id: ~A"
              balance id name parent-id))))

;;; 定义账户的 repo 接口。
(defgeneric create-account (repo name parent-id)
  (:documentation "创建一个账户。"))

(defgeneric delete-account (repo id)
  (:documentation "删除指定的账户。"))

(defgeneric get-account (repo id)
  (:documentation "基于 ID 获取账户。"))

(defgeneric get-by-name (repo name)
  (:documentation "基于名称来查找账户。"))

(defgeneric list-by-parent-id (repo parent-id)
  (:documentation "查询指定的 parent_id 的账户。"))

(defgeneric lock-account-by-id (repo id)
  (:documentation "用主键锁定一行账户记录。"))
