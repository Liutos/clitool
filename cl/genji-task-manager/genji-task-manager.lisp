;;;; genji-task-manager.lisp

(in-package #:genji-task-manager)

;;; "genji-task-manager" goes here. Hacks and glory await!

(defun create (request)
  "往Redis中记录本次单条爬虫任务的提交情况"
  (eloquent.mvc.controller:json-body-bind
      ((extras "extras")
       (uri "uri"))
      request
    (redis:with-connection ()
      (let ((value (red:get uri)))
        (if value
            (eloquent.mvc.response:respond "" :status 204)
            (progn
              (let ((queue (cl-httpsqs:make-queue "127.0.0.1" 1219)))
                (cl-httpsqs:enqueue uri "genji-task-test" queue))
              (red:set uri (json:encode-json-to-string extras))
              (eloquent.mvc.response:respond
               uri
               :status 201)))))))

(defun retrive (request)
  "从Redis中查找KEY对应的附加数据"
  (declare (ignorable request))
  (eloquent.mvc.controller:query-string-bind ((key "key"))
      request
    (redis:with-connection ()
      (let ((value (red:get key)))
        (format t "~S~%" key)
        (format t "~S~%" value)
        (eloquent.mvc.response:respond
         value)))))
