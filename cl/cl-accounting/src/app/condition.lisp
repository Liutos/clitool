(in-package #:cl-accounting.app)

(define-condition <business-error> (error)
  ((msg
    :initarg :msg
    :reader msg-of))
  (:documentation "出于业务一致性的目的拦截请求的异常。")
  (:report (lambda (condition stream)
             (format stream "~A" (slot-value condition 'msg)))))
