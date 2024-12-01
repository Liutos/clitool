(in-package :cl-accounting.web)

(defvar *acceptor* nil)

(defun install-routes ()
  "注册路由规则。"
  (push (hunchentoot:create-prefix-dispatcher "/api/account/create" #'create-account)
        hunchentoot:*dispatch-table*))

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
