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
                 (to-account-id (cl-accounting.entity:transfer-to-account-id transfer)))
            (push (make-transfer-list-item
                   :amount (cl-accounting.entity:transfer-amount transfer)
                   :comment (cl-accounting.entity:transfer-comment transfer)
                   :created-at (cl-accounting.entity:transfer-created-at transfer)
                   :from-account-name (gen-full-account-name from-account-id account-repo)
                   :id (cl-accounting.entity:transfer-id transfer)
                   :to-account-name (gen-full-account-name to-account-id account-repo))
                  items)))

        ;; 最后返回被填充的结果。
        items))))

(defun gen-full-account-name (account-id account-repo)
  "生成完整层级的账户名称。"
  (let* ((account (cl-accounting.entity:get-account account-repo account-id))
         (account-name (cl-accounting.entity:account-name account))
         (parent-id (cl-accounting.entity:account-parent-id account)))
    (when (zerop parent-id)
      ;; 已经是顶层了，那么账户的完整名称就是自己。
      (return-from gen-full-account-name account-name))

    (let ((parent-name (gen-full-account-name parent-id account-repo)))
      (format nil "~A:~A" parent-name account-name))))
