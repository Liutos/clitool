;;; SAK means Swiss army knife
(in-package :cl-user)

(defpackage #:com.liutos.sak
  (:use #:cl))

(in-package #:com.liutos.sak)

(defun vertical-let/aux (op body)
  (let ((stack '()))
    ;; 处理BODY中的所有元素将它们归类为binding和form塞入STACK中
    (let ((lst body))
      (while lst
        (let ((e (pop lst)))
          (cond ((eq e :with)
                 (let* ((var (pop lst))
                        (equal-sign (pop lst))
                        (val (pop lst)))
                   (assert (symbolp var))
                   (assert (string= (symbol-name equal-sign) "="))
                   (push (cons :binding (list var val)) stack)))
                (t (push (cons :form e) stack))))))
    ;; 处理STACK中的元素生成嵌套的LET表达式
    (let ((bindings '())
          (forms '())
          (last-type nil))
      (while stack
        (let ((e (pop stack)))
          (cond ((eq (car e) :binding)
                 (push (cdr e) bindings)
                 (setf last-type :binding))
                (t
                 (when (eq last-type :binding)
                   (setf forms
                         `((,op ,bindings ,@forms)))
                   (setf bindings '()))
                 (push (cdr e) forms)
                 (setf last-type :form)))))
      `(,op ,bindings
         ,@forms))))

(defmacro vertical-let (&body body)
  "不需要不停缩进的LET"
  (vertical-let/aux 'let body))

(defmacro vertical-let* (&body body)
  "不需要不停缩进的LET*"
  (vertical-let/aux 'let* body))

(defmacro while (test &body body)
  "C语言风格的while循环"
  `(loop
      :while ,test
      :do (progn ,@body)))
