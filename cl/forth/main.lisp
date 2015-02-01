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
                 (* (* l1 l2)))))
      (vector-push-extend val stack)
      (values seq stack))))

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
                     (return-from aux (nreverse body)))
                   (push ins body)
                   (aux seq body))
                 (nreverse body))))
    (let ((name (pop seq))
          (body (aux seq '())))
      (setf (gethash name *forth-dict*) body))))

;;; 根据给定指令的类型执行不同的逻辑
(defun interp (ins stack seq)
  "Interpret a single instruction."
  (cond ((numberp ins) (interp-number ins stack seq))
        ((eq '|.| ins) (interp-point ins stack seq))
        ((eq 'dup ins) (interp-dup ins stack seq)
         (values seq stack))
        ((eq 'swap ins) (interp-swap ins stack seq))
        ((eq '|:| ins) (interp-define seq))
        ((eq '* ins) (interp-arith ins stack seq))
        ((eq 'tuck ins) (interp-tuck ins stack seq))
        ((eq 'drop ins) (interp-drop ins stack seq))
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
