(in-package :cl-accounting.web)

(defun tree-account ()
  "返回账户树形选项列表。"
  (cl-accounting.infra:with-connection (connection)
    (let* ((handler
             (make-instance 'cl-accounting.app:<tree-account-handler>
                            :uow (cl-accounting.repo:new-mysql-unit-of-work connection))))
      (let* ((options (cl-accounting.app:run-handler handler))
             (data (make-hash-table)))
        (setf (gethash "options" data) options)
        ;; TODO: 将这里的 with-output-to-string 的用法改为一个装饰器或中间件。
        (with-output-to-string (s)
          (setf (hunchentoot:content-type*) "Content-Type: application/json")
          (let ((result (list
                         "data" data
                         "msg" "success"
                         "status" 0))) ; 类似于 UNIX 的惯例，0 表示【没有异常】。
            (yason:encode (alexandria:plist-hash-table result) s)))))))
