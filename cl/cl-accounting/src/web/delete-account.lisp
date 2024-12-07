(in-package :cl-accounting.web)

(defun delete-account ()
  "基于 POST 中传入的 ID 来删除一个账户。"
  (let* ((raw-post-data (hunchentoot:raw-post-data
                         :external-format 'utf-8
                         :force-text t))
         (parsed (yason:parse raw-post-data))
         (account-id (gethash "account_id" parsed))
         (connection (cl-accounting.infra:get-connection))
         (account-repo (cl-accounting.repo:new-mysql-account-repo connection))
         (handler
           (make-instance 'cl-accounting.app:<delete-account-handler>
                          :account-id account-id
                          :account-repo account-repo)))
    ;; TODO: 将这里的 with-output-to-string 的用法改为一个装饰器或中间件。
    (with-output-to-string (*standard-output*)
      (cl-accounting.app:run-handler handler)
      (setf (hunchentoot:content-type*) "Content-Type: application/json")
      (let ((result (list
                     "msg" "success"
                     "status" 0))) ; 类似于 UNIX 的惯例，0 表示【没有异常】。
        (yason:encode (alexandria:plist-hash-table result) *standard-output*)))))
