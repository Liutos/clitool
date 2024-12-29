(in-package #:cl-accounting.app)

(defclass <list-transfer-handler> ()
  ((uow
    :initarg :uow))
  (:documentation "处理转账列表的请求。"))

;;; 转账记录列表的 view-model。
(defstruct transfer-list-item
  amount
  comment
  created-at
  from-account-name
  id
  to-account-name)

(defmethod run-handler ((handler <list-transfer-handler>))
  (with-slots (uow)
      handler
    (let ((account-repo (get-account-repo uow))
          items
          (transfer-repo (get-transfer-repo uow)))
      (let ((transfer-list
              (cl-accounting.entity:list-transfer transfer-repo)))
        (dolist (transfer transfer-list)
          (let* ((from-account-id (cl-accounting.entity:transfer-from-account-id transfer))
                 (from-account (cl-accounting.entity:get-account account-repo from-account-id))
                 (to-account-id (cl-accounting.entity:transfer-to-account-id transfer))
                 (to-account (cl-accounting.entity:get-account account-repo to-account-id)))
            (push (make-transfer-list-item
                   :amount (cl-accounting.entity:transfer-amount transfer)
                   :comment (cl-accounting.entity:transfer-comment transfer)
                   :created-at (cl-accounting.entity:transfer-created-at transfer)
                   :from-account-name (cl-accounting.entity:account-name from-account)
                   :id (cl-accounting.entity:transfer-id transfer)
                   :to-account-name (cl-accounting.entity:account-name to-account))
                  items)))

        ;; 最后返回被填充的结果。
        items))))
