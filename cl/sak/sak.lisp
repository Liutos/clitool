;;; SAK means Swiss army knife
(in-package :cl-user)

(defpackage #:com.liutos.sak
  (:use #:cl))

(in-package #:com.liutos.sak)

(defparameter *memo* (make-hash-table))

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

(defun floyd-sample (m n)
  "Floyd抽样算法

算法参考自这篇[文章](http://book.51cto.com/art/201104/258180.htm)"
  (check-type m integer)
  (check-type n integer)
  (let (result
        (s (make-hash-table)))
    (loop
       :for j :from (1+ (- n m)) :to n
       :do (let ((rn (1+ (random j))))
             (if (gethash rn s)
                 (setf (gethash j s) t)
                 (setf (gethash rn s) t))))
    (maphash #'(lambda (k v)
                 (declare (ignorable v))
                 (push k result))
             s)
    result))

(defun memoize (f biz)
  ;; 确保BIZ对应的作为缓存的哈希表存在
  (multiple-value-bind (val foundp)
      (gethash biz *memo*)
    (declare (ignorable val))
    (when (not foundp)
      (setf (gethash biz *memo*)
            (make-hash-table :test 'equal))))

  (let ((name (gensym)))
    (lambda (&rest args)
      (catch name
        (let ((cache (gethash biz *memo*)))
          (multiple-value-bind (val foundp)
              (gethash args cache)
            (when foundp
              (throw name val)))

          (let ((val (apply f args)))
            (setf (gethash args cache) val)
            val))))))
