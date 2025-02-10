(in-package #:cl-accounting.app)

(defclass <create-transfer-handler> ()
  ((amount
    :initarg :amount)
   (comment
    :initarg :comment)
   (from-account-id
    :initarg :from-account-id)
   (to-account-id
    :initarg :to-account-id)
   (transfer-at
    :documentation "转账发生的日期。"
    :initarg :transfer-at)
   (uow
    :initarg :uow))
  (:documentation "处理新增转账的请求。"))

(defmethod run-handler ((handler <create-transfer-handler>))
  (with-slots (amount comment from-account-id to-account-id transfer-at uow)
      handler
    ;; 参数校验。
    (unless amount
      (error '<business-error> :msg "参数 amount 不能为空"))

    (unless (stringp amount)
      (error '<business-error> :msg "参数 amount 必须是字符串类型"))

    (unless transfer-at
      (error '<business-error> :msg "参数 transfer_at 不能为空。"))

    (unless (stringp transfer-at)
      (error '<business-error> :msg "参数 transfer_at 必须是字符串类型。"))
    ;; 校验 ID 有效性。
    (let ((account-repo (get-account-repo uow))
          (parsed-transfer-at (parse-date transfer-at))
          (transfer-repo (get-transfer-repo uow)))
      ;; TODO: 将 begin-transaction/handler-case 等用法封装为一个高阶函数或者宏。
      (begin-transaction uow)
      (handler-case
          (let ((from-account
                  (cl-accounting.entity:lock-account-by-id account-repo from-account-id))
                (to-account
                  (cl-accounting.entity:lock-account-by-id account-repo to-account-id)))
            (unless from-account
              (error '<business-error> :msg (format nil "找不到 ID 为 ~D 的账户" from-account-id)))

            (unless to-account
              (error '<business-error> :msg (format nil "找不到 ID 为 ~D 的账户" to-account-id)))

            (cl-accounting.entity:create-transfer
             transfer-repo
             amount
             from-account-id
             to-account-id
             :comment comment
             :transfer-at parsed-transfer-at)
            ;; 计算余额并更新到 t_account 表中。
            (let ((from-account-balance (compute-account-balance from-account-id transfer-repo))
                  (to-account-balance (compute-account-balance to-account-id transfer-repo)))
              (cl-accounting.entity:update-balance account-repo from-account-id from-account-balance)
              (cl-accounting.entity:update-balance account-repo to-account-id to-account-balance))

            (commit-transaction uow))
        (t (var)
          ;; 回滚数据库事务，并继续往上抛出异常。
          (rollback-transaction uow)
          (error var))))))

(defun compute-account-balance (account-id transfer-repo)
  "计算给定账户 ACCOUNT-ID 的余额。"
  (let ((flow-in (cl-accounting.entity:list-by-to-account-id transfer-repo account-id))
        (flow-out (cl-accounting.entity:list-by-from-account-id transfer-repo account-id)))
    ;; flow-in 存储的是流入 account-id 的转账记录，那么它的金额会导致目标账户余额增加，flow-out 则相反。
    (flet ((sum-flow (transfers)
             (apply #'+ (mapcar #'(lambda (transfer)
                                    (cl-accounting.entity:transfer-amount transfer))
                                transfers))))
      (let ((total-in (sum-flow flow-in))
            (total-out (sum-flow flow-out)))
        (- total-in total-out)))))

(defun parse-date (date-string)
  "将日期字符串 DATE-STRING 按照 yyyy-mm-dd 的格式解析成 local-time:timestamp 对象。"
  (cl-ppcre:register-groups-bind (year month date)
      ("^([0-9]{4})-([0-9]{2})-([0-9]{2})$" date-string)
    (unless (and year month date)
      (error '<business-error> :msg "日期字符串必须是 yyyy-mm-dd 的格式。"))

    (local-time:encode-timestamp 0 0 0 0 (parse-integer date) (parse-integer month) (parse-integer year))))
