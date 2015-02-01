(in-package :common-lisp)

(defpackage liutos.cli.forth
  (:use :common-lisp)
  (:export :forth))

(in-package :liutos.cli.forth)

(defparameter *forth-dict*
  (make-hash-table))

;;; 实现四则运算，包括了加减乘除
(defun interp-arith (ins stack seq)
  "Arithmetic operations."
  (let ((l2 (vector-pop stack))
        (l1 (vector-pop stack)))
    (let ((val (case ins
                 (+ (+ l1 l2))
                 (- (- l1 l2))
                 (* (* l1 l2))
                 (/ (/ l1 l2))
                 (= (= l1 l2)))))
      (vector-push-extend val stack)
      (values seq stack))))

(defun interp-cr (ins stack seq)
  "Print a carriage return character."
  (declare (ignorable ins stack seq))
  (format t "~%")
  (values seq stack))

(defun interp-drop (ins stack seq)
  "Remove the top of stack."
  (declare (ignore ins))
  (vector-pop stack)
  (values seq stack))

(defun interp-dup (ins stack seq)
  "Duplicate the top of stack."
  (declare (ignore ins))
  (let ((e (aref stack (1- (fill-pointer stack)))))
    (vector-push-extend e stack)
    (values seq stack)))

(defun interp-number (ins stack seq)
  "Push number onto stack."
  (vector-push-extend ins stack)
  (values seq stack))

(defun interp-point (ins stack seq)
  "Pop and print the top of stack."
  (declare (ignore ins))
  (let ((e (vector-pop stack)))
    (format t "~A" e))
  (values seq stack))

(defun interp-tuck (ins stack seq)
  "Push the last but two element onto stack."
  (declare (ignore ins))
  (let ((e (aref stack (- (fill-pointer stack) 2))))
    (vector-push-extend e stack)
    (values seq stack)))

(defun interp-swap (ins stack seq)
  "Swap the top two elements on stack."
  (declare (ignore ins))
  (let ((l1 (vector-pop stack))
        (l2 (vector-pop stack)))
    (vector-push-extend l1 stack)
    (vector-push-extend l2 stack)
    (values seq stack)))

;;; 读取过程名
;;; 读取过程体定义，直到遇到分号为止
;;; 把过程体和过程名绑定到全局环境中
(defun interp-define (seq)
  "Define a new procedure."
  (labels ((aux (seq body)
             (if seq
                 (let ((ins (pop seq)))
                   (when (eq '|;| ins)
                     (return-from aux
                       (values seq (nreverse body))))
                   (push ins body)
                   (aux seq body))
                 (values seq (nreverse body)))))
    (let ((name (pop seq)))
      (multiple-value-bind (seq body)
          (aux seq '())
        (setf (gethash name *forth-dict*) body)
        (values seq body)))))

;;; 返回包括接下来的指令序列和过程体
(defun interp-begin-aux2 (seq)
  "Pop instructions until got REPEAT."
  (let ((rest '()))
    (loop
       :for (ins . tail) :on seq
       :until (eq 'repeat ins)
       :do (push ins rest)
       :finally (return-from interp-begin-aux2
                  (values tail (nreverse rest))))))

;;; 遇到UNTIL时终止，或
;;; 遇到WHILE时继续读取，直到遇到REPEAT
;;; 返回包括：指令序列，循环过程体，条件部分，语法类型(:UNTIL|:REPEAT)
(defun interp-begin-aux (seq body)
  (let ((ins (pop seq)))
    (cond ((eq 'until ins)
           (let ((cnd (pop body)))
             (values seq (nreverse body) (list cnd) :until)))
          ((eq 'while ins)
           (let ((cnd (pop body)))
             (multiple-value-bind (seq rest)
                 (interp-begin-aux2 seq)
               (values seq
                       (list (nreverse body) rest)
                       (list cnd) :repeat))))
          (t (interp-begin-aux seq (cons ins body))))))

(defun interp-begin-until (stack body cnd)
  "Execute body until given condition reached."
  (forth-eat body stack)
  (forth-eat cnd stack)
  (let ((e (vector-pop stack)))
    (unless e
      (interp-begin-until stack body cnd))))

(defun interp-begin-repeat (stack body-before body-after cnd)
  "Execute the first part, check condition and execute the second part if condition is not satisfied."
  (forth-eat body-before stack)
  (forth-eat cnd stack)
  (let ((e (vector-pop stack)))
    (unless e
      (forth-eat body-after stack)
      (interp-begin-repeat stack body-before body-after cnd))))

;;; 包括了了两种语法
;;; 第一种：BEGIN ... cond UNTIL
;;; 第二种：BEGIN ... cond WHILE ... REPEAT
(defun interp-begin (ins stack seq)
  "Loop"
  (declare (ignore ins))
  (multiple-value-bind (seq bodys cnd kind)
      (interp-begin-aux seq '())
    (case kind
      (:until (interp-begin-until stack bodys cnd))
      (:repeat (interp-begin-repeat stack (first bodys) cnd (second bodys))))
    (values seq stack)))

;;; 根据给定指令的类型执行不同的逻辑
(defun interp (ins stack seq)
  "Interpret a single instruction."
  (cond ((numberp ins) (interp-number ins stack seq))
        ((eq '|.| ins) (interp-point ins stack seq))
        ((eq 'cr ins) (interp-cr ins stack seq))
        ((eq 'dup ins) (interp-dup ins stack seq))
        ((eq 'swap ins) (interp-swap ins stack seq))
        ((eq '|:| ins) (interp-define seq))
        ((member ins '(+ - * / =)) (interp-arith ins stack seq))
        ((eq 'tuck ins) (interp-tuck ins stack seq))
        ((eq 'drop ins) (interp-drop ins stack seq))
        ((eq 'begin ins) (interp-begin ins stack seq))
        ((gethash ins *forth-dict*)
         (let ((stack (nth-value 1 (forth-eat (gethash ins *forth-dict*) stack))))
           (values seq stack)))
        (t (error ins))))

;;; 依次读取seq中的各个元素，并调用interp函数进行解释
;;; stack是参数栈，会随着指令的执行而改变
(defun forth-eat (seq stack)
  "Iterate and interprete the given sequence."
  (if seq
      (let* ((ins (pop seq)))
        (multiple-value-bind (seq stack)
            (interp ins stack seq)
          (forth-eat seq stack)))
      (values '() stack)))

;;; forth: [t] -> IO ()
;;; 遍历给定的指令序列并解释每一个遇到的元素
(defun forth (seq)
  "A fast and portable Forth system."
  (let ((stack (make-array 10 :fill-pointer 0)))
    (forth-eat seq stack)))
