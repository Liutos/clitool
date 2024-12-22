(in-package #:cl-accounting.repo)

(defclass <mysql-unit-of-work> ()
  ((connection
    :initarg :connection))
  (:documentation "Unit-Of-Work 模式的实现者。"))

(defmethod get-account-repo ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (new-mysql-account-repo connection)))

(defmethod get-transfer-repo ((uow <mysql-unit-of-work>))
  (with-slots (connection) uow
    (new-mysql-transfer-repo connection)))

(defun new-mysql-unit-of-work (connection)
  (make-instance '<mysql-unit-of-work>
                 :connection connection))
