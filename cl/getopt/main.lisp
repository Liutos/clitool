(in-package :common-lisp)

(defpackage :liutos.cli.getopt
  (:use :common-lisp)
  (:export :getopt))

(in-package :liutos.cli.getopt)

(define-condition getopt-empty ()
  ())

(defun getopt-parse (optstring parameters)
  (let ((tbl (make-hash-table))
        (params parameters))
    (with-input-from-string (s optstring)
      (loop
         :for opt = (read-char s nil)
         :while opt
         :do (let ((next (peek-char nil s)))
               (cond ((char= #\: next)
                      (read-char s nil)
                      (setf (gethash opt tbl) t))
                     (t (setf (gethash opt tbl) nil)))))
      (lambda ()
        (when (null params)
          (error 'getopt-empty))
        (let* ((opt (char (pop params) 1))
               (status (gethash opt tbl)))
          (values opt (if status (pop params) nil)))))))

(defun getopt-body-parse (val body)
  (mapcar #'(lambda (clause)
              (destructuring-bind (opt pars . body)
                  clause
                `(,opt (funcall #'(lambda ,pars ,@body) ,val))))
          body))

(defmacro getopt (optstring parameters &body body)
  "Parse command options (enhanced)"
  (let ((opt (gensym))
        (val (gensym))
        (par (gensym)))
    `(let ((,par (getopt-parse ,optstring ,parameters)))
       (handler-case
           (loop
              :for (,opt ,val) = (multiple-value-list (funcall ,par))
              :do (case ,opt
                    ,@(getopt-body-parse val body)))
         (getopt-empty ())))))

;;; 调用getopt宏的示例代码如下
(getopt "C:c:" '("-C" "/etc/cups/cupsd.conf")
  (#\C (path)
       (format t "~A~%" path)))
