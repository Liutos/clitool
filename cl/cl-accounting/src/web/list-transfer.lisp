(in-package :cl-accounting.web)

;;; 为了可以让 yason 将自定义的 <transfer> 类型序列化为 JSON，需要定义相应的方法。
(defmethod yason:encode ((object cl-accounting.app:transfer-list-item) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "amount" (cl-accounting.app:transfer-list-item-amount object))
      (yason:encode-object-element "comment" (cl-accounting.app:transfer-list-item-comment object))
      (yason:encode-object-element "created_at"
                                   (let ((created-at (cl-accounting.app:transfer-list-item-created-at object)))
                                     (local-time:format-timestring
                                      nil
                                      (local-time:universal-to-timestamp created-at)
                                      :format (list :year "-" :month "-" :day " " :hour ":" :min ":" :sec))))
      (yason:encode-object-element "from_account_name" (cl-accounting.app:transfer-list-item-from-account-name object))
      (yason:encode-object-element "id" (cl-accounting.app:transfer-list-item-id object))
      (yason:encode-object-element "to_account_name" (cl-accounting.app:transfer-list-item-to-account-name object)))))

(defun list-transfer ()
  "返回转账记录列表。"
  (let* ((connection (cl-accounting.infra:get-connection))
         (handler
           (make-instance 'cl-accounting.app:<list-transfer-handler>
                          :uow (cl-accounting.repo:new-mysql-unit-of-work connection))))
    (let ((transfer-list (cl-accounting.app:run-handler handler)))
      ;; TODO: 将这里的 with-output-to-string 的用法改为一个装饰器或中间件。
      (with-output-to-string (s)
        (setf (hunchentoot:content-type*) "Content-Type: application/json")
        (let* ((data (let ((data (make-hash-table)))
                       ;; TODO: 这里不应该直接用 <transfer> 类型，而是应当提供一个 view-model 类型。
                       (setf (gethash "items" data) transfer-list
                             (gethash "total" data) (length transfer-list))
                       data))
               (result (list
                        "data" data
                        "msg" "success"
                        "status" 0))) ; 类似于 UNIX 的惯例，0 表示【没有异常】。
          (yason:encode (alexandria:plist-hash-table result) s))))))
