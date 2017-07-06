(in-package #:gnu-find)

(defun stringify-and-expressions (expressions)
  (check-type expressions cons)
  (with-output-to-string (s)
    (loop
       :for exprs :on expressions
       :do (progn
             (format s "~A" (stringify-expression (first exprs)))
             (when (rest exprs)
               (format s " -a"))))))

(defun stringify-or-expressions (expressions)
  (check-type expressions cons)
  (with-output-to-string (s)
    (loop
       :for exprs :on expressions
       :do (progn
             (format s "~A" (stringify-expression (first exprs)))
             (when (rest exprs)
               (format s " -o"))))))

(defun stringify-expression (expression)
  (check-type expression list)
  (destructuring-bind (test argument) expression
    (format nil " -~A ~S" (stringify-test test) argument)))

(defun stringify-expressions (expressions)
  (check-type expressions list)
  (with-output-to-string (s)
    (dolist (expression expressions)
      (let ((operator (first expression)))
        (case operator
          (and (format s "~A" (stringify-and-expressions (rest expression))))
          (or (format s "~A" (stringify-or-expressions (rest expression))))
          (t (format s "~A" (stringify-expression expression))))))))

(defun stringify-test (test)
  (check-type test (or string symbol))
  (etypecase test
    (string test)
    (symbol (string-downcase (symbol-name test)))))

(defun gnu-find (starting-point &rest expressions)
  (declare (ignorable expressions))
  (check-type starting-point string)
  (let* ((command (format nil "find ~A~A" starting-point (stringify-expressions expressions)))
         (files (with-output-to-string (*standard-output*)
                  (uiop:run-program command :output t)))
         (files (split-sequence:split-sequence #\Newline files))
         (files (remove-if #'(lambda (s)
                               (string= s ""))
                           files)))
    files))
