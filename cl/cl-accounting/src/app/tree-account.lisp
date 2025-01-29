(in-package #:cl-accounting.app)

(defclass <tree-account-handler> ()
  ((uow
    :initarg :uow))
  (:documentation "生成树形结构的账户列表。"))

(defmethod run-handler ((handler <tree-account-handler>))
  (with-slots (uow)
      handler
    (let ((account-repo (get-account-repo uow)))
      (gen-children (cl-accounting.entity:list-all-accounts account-repo)
                    0))))

(defun gen-children (all-accounts parent-id)
  "从数据源 all-accounts 中递归地构造出以 ID 为 parent-id 的账户为根节点的账户树。"
  (let (result
        (sub-accounts (remove-if #'(lambda (account)
                                     (/= (cl-accounting.entity:account-parent-id account)
                                         parent-id))
                                 all-accounts)))
    ;; 对每一个一级的子账户，递归地找出它们的 children 列表。
    (dolist (sub-account sub-accounts)
      (let ((children (gen-children all-accounts (cl-accounting.entity:account-id sub-account)))
            (node (make-hash-table)))
        (setf (gethash "children" node) children
              (gethash "label" node) (cl-accounting.entity:account-name sub-account)
              (gethash "value" node) (cl-accounting.entity:account-id sub-account))
        (push node result)))

    (nreverse result)))
