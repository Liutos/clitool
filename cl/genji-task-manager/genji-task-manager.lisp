;;;; genji-task-manager.lisp

(in-package #:genji-task-manager)

;;; "genji-task-manager" goes here. Hacks and glory await!

(defun create (request)
  "往Redis中记录本次单条爬虫任务的提交情况"
  (eloquent.mvc.controller:json-body-bind
      ((extras "extras")
       (uri "uri"))
      request
    (let ((value (red:get uri)))
      (if value
          (eloquent.mvc.response:respond "" :status 204)
          (progn
            (let ((data (append extras (list (cons "entry" uri))))
                  (queue (cl-httpsqs:make-queue "127.0.0.1" 1219)))
              (cl-httpsqs:enqueue (json:encode-json-to-string data) "genji-task" queue))
            (red:set uri (json:encode-json-to-string extras))
            (eloquent.mvc.response:respond
             uri
             :status 201))))))

(defun restore (request)
  "将一条已经处理的任务重新推入队列中"
  (eloquent.mvc.controller:json-body-bind ((uri "uri"))
      request
    (let ((value (red:get uri)))
      (if value
          (progn
            (let ((queue (cl-httpsqs:make-queue "127.0.0.1" 1219)))
              (cl-httpsqs:enqueue value "genji-task" queue))
            (eloquent.mvc.response:respond
             uri
             :status 201))
          (eloquent.mvc.response:respond "" :status 204)))))

(defun retrive (request)
  "从Redis中查找KEY对应的附加数据"
  (declare (ignorable request))
  (eloquent.mvc.controller:query-string-bind ((key "key"))
      request
    (let ((value (red:get key)))
      (format t "~S~%" key)
      (format t "~S~%" value)
      (eloquent.mvc.response:respond
       value))))

(defun with-redis-connection (request next &key config)
  "打开与Redis的连接"
  (let ((port (eloquent.mvc.config:get config "genji-task-manager" "redis-port")))
    (redis:with-connection (:port port)
      (funcall next request))))
