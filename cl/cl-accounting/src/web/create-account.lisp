(in-package :cl-accounting.web)

(defun create-account ()
  "解析 POST 中的数据来创建一个账户。"
  (let* ((raw-post-data (hunchentoot:raw-post-data
                         :external-format 'utf-8
                         :force-text t))
         (parsed (yason:parse raw-post-data))
         (name (gethash "name" parsed))
         (parent-id (gethash "parent_id" parsed))
         (connection (cl-accounting.infra:get-connection))
         (account-repo (cl-accounting.repo:new-mysql-account-repo connection))
         (handler
           (make-instance 'cl-accounting.app:<create-account-handler>
                          :account-repo account-repo
                          :name name
                          :parent-id parent-id)))
    ;; TODO: 将这里的 with-output-to-string 的用法改为一个装饰器或中间件。
    (with-output-to-string (*standard-output*)
      ;; TODO: 将这里捕捉异常的代码剥离为装饰器或中间件。
      (handler-case
          (progn
            (cl-accounting.app:run-handler handler)
            (setf (hunchentoot:content-type*) "Content-Type: application/json")
            (let ((result (list
                           "msg" "success"
                           "status" 0)))        ; 类似于 UNIX 的惯例，0 表示【没有异常】。
              (yason:encode (alexandria:plist-hash-table result) *standard-output*)))

        (cl-accounting.app:<business-error> (c)
          (setf (hunchentoot:content-type*) "Content-Type: application/json")
          (let ((result (list
                         "msg" (cl-accounting.app:msg-of c)
                         "status" 1)))
            (yason:encode (alexandria:plist-hash-table result) *standard-output*)))))))
