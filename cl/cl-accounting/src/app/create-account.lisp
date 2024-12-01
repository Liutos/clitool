(in-package #:cl-accounting.app)

(defclass <create-account-handler> ()
  ((account-repo
    :initarg :account-repo)
   (name
    :initarg :name)
   (parent-id
    :initarg :parent-id))
  (:documentation "处理创建账户的请求。"))

(defmethod run-handler ((handler <create-account-handler>))
  (with-slots (account-repo name parent-id)
      handler
    ;; 参数校验。
    (unless name
      (error '<business-error> :msg "参数 name 不能为空"))

    (when parent-id
      (let ((parent-account
              (cl-accounting.entity:get-account account-repo parent-id)))
        (unless parent-account
          (error '<business-error> :msg (format nil "找不到 ID 为 ~D 的账户" parent-id)))))

    ;; 业务层面的校验，例如不能重名。
    (let ((existing (cl-accounting.entity:get-by-name account-repo name)))
      (when existing
        (error '<business-error> :msg (format nil "账户【~A】已经存在，请勿重复创建" name))))

    (cl-accounting.entity:create-account
     account-repo
     name
     parent-id)))
