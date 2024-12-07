(in-package #:cl-accounting.app)

(defclass <delete-account-handler> ()
  ((account-id
    :initarg :account-id)
   ;; TODO: 这里不是要一个具体的 account-repo，而是要一个 unit-of-work 对象，在启动了数据库事务后、再获取一个 repo 对象。
   (account-repo
    :initarg :account-repo))
  (:documentation "处理删除账户的请求。"))

(defmethod run-handler ((handler <delete-account-handler>))
  (with-slots (account-id account-repo)
      handler
    ;; 参数校验。
    (unless account-id
      (error '<business-error> :msg "参数 account_id 不能为空"))

    ;; TODO: 这里要给待删除的 account 上锁。
    (let ((account (cl-accounting.entity:get-account account-repo account-id)))
      (unless account
        (error '<business-error> :msg (format nil "找不到 ID 为 ~A 的账户" account-id)))

      (let ((sub-accounts (cl-accounting.entity:list-by-parent-id account-repo account-id)))
        (when (> (length sub-accounts) 0)
          (error '<business-error> :msg (format nil "ID 为 ~s 的账户有子账户，不能删除" account-id))))
      
      (cl-accounting.entity:delete-account account-repo account-id))))
