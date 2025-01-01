(in-package :cl-accounting.web)

(defun create-transfer ()
  "解析 POST 中的数据来创建一条转账记录。"
  (let* ((raw-post-data (hunchentoot:raw-post-data
                         :external-format 'utf-8
                         :force-text t))
         (parsed (yason:parse raw-post-data))
         (amount (gethash "amount" parsed))
         (comment (gethash "comment" parsed))
         (from-account-id (gethash "from_account_id" parsed))
         (to-account-id (gethash "to_account_id" parsed))
         (connection (cl-accounting.infra:get-connection))
         (handler
           (make-instance 'cl-accounting.app:<create-transfer-handler>
                          :amount amount
                          :comment comment
                          :from-account-id from-account-id
                          :to-account-id to-account-id
                          :uow (cl-accounting.repo:new-mysql-unit-of-work connection))))
    ;; TODO: 将这里的 with-output-to-string 的用法改为一个装饰器或中间件。
    (with-output-to-string (s)
      (cl-accounting.app:run-handler handler)
      (setf (hunchentoot:content-type*) "Content-Type: application/json")
      (let ((result (list
                     "msg" "success"
                     "status" 0))) ; 类似于 UNIX 的惯例，0 表示【没有异常】。
        (yason:encode (alexandria:plist-hash-table result) s)))))
