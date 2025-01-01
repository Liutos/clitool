(in-package #:cl-accounting.app)

(defclass <list-account-handler> ()
  ((uow
    :initarg :uow))
  (:documentation "处理账号选项列表的请求。"))

(defmethod run-handler ((handler <list-account-handler>))
  (with-slots (uow)
      handler
    (let ((account-repo (get-account-repo uow)))
      (cl-accounting.entity:list-all-accounts account-repo))))
