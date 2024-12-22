(in-package #:cl-accounting.app)

(defclass <create-transfer-handler> ()
  ((amount
    :initarg :amount)
   (from-account-id
    :initarg :from-account-id)
   (to-account-id
    :initarg :to-account-id)
   (uow
    :initarg :uow))
  (:documentation "处理新增转账的请求。"))

(defmethod run-handler ((handler <create-transfer-handler>))
  (with-slots (amount from-account-id to-account-id uow)
      handler
    ;; 参数校验。
    (unless amount
      (error '<business-error> :msg "参数 amount 不能为空"))

    (unless (stringp amount)
      (error '<business-error> :msg "参数 amount 必须是字符串类型"))
    ;; 校验 ID 有效性。
    (let ((account-repo (get-account-repo uow))
          (transfer-repo (get-transfer-repo uow)))
      (let ((from-account
              (cl-accounting.entity:get-account account-repo from-account-id))
            (to-account
              (cl-accounting.entity:get-account account-repo to-account-id)))
        (unless from-account
          (error '<business-error> :msg (format nil "找不到 ID 为 ~D 的账户" from-account-id)))

        (unless to-account
          (error '<business-error> :msg (format nil "找不到 ID 为 ~D 的账户" to-account-id)))

        (cl-accounting.entity:create-transfer
         transfer-repo
         amount
         from-account-id
         to-account-id)))))
