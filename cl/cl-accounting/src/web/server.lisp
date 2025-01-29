(in-package :cl-accounting.web)

(defvar *acceptor* nil)

(defun handle-business-error (func)
  "捕获调用函数 func 时抛出的 <business-error> 异常，并将其转换为 JSON 格式的返回。"
  (handler-case
      (funcall func)
    (cl-accounting.app:<business-error> (c)
      (setf (hunchentoot:content-type*) "Content-Type: application/json")
      (let ((result (list
                     "msg" (cl-accounting.app:msg-of c)
                     "status" 1)))
        (with-output-to-string (*standard-output*)
          (yason:encode (alexandria:plist-hash-table result) *standard-output*))))))

(defun install-routes ()
  "注册路由规则。"
  (let ((path-handlers
          (list (list "/api/account/create" #'create-account)
                (list "/api/account/delete" #'delete-account)
                (list "/api/account/list" #'list-account)
                (list "/api/account/tree" #'tree-account)
                (list "/api/transfer/create" #'create-transfer)
                (list "/api/transfer/list" #'list-transfer)
                (list "/index.html" (make-static-file-handler "/app/src/web/html/index.html")))))
    (dolist (info path-handlers)
      (let ((path (first info))
            (handler (second info)))
        (push (hunchentoot:create-prefix-dispatcher path (lambda ()
                                                           (handle-business-error handler)))
              hunchentoot:*dispatch-table*)))

    ;; 用正则表达式来匹配所有处理的 .css、.js 文件请求。
    (push (hunchentoot:create-regex-dispatcher "/.*\\.css" #'respond-amis-css-file)
          hunchentoot:*dispatch-table*)
    (push (hunchentoot:create-regex-dispatcher "/.*\\.js" #'respond-amis-js-file)
          hunchentoot:*dispatch-table*)))

(defun init ()
  "初始化一个 WEB 服务器接收请求。"
  (install-routes)
  (setf *acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port 4242)))

(defun start ()
  "启动这个 WEB 服务器。"
  (swank:create-server :port 4006 :dont-close t)
  (format t "swank 服务器启动完毕。~%")
  (hunchentoot:start *acceptor*)
  (format t "WEB 服务器启动完毕。~%")
  (let* ((all-threads (sb-thread:list-all-threads))
         (listener (flet ((find-listener (th)
                            (string= (sb-thread:thread-name th) "hunchentoot-listener-*:4242")))
                     (find-if #'find-listener all-threads))))
    (sb-thread:join-thread listener)))
