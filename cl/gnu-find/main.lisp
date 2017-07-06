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
  "Return file names matching EXPRESSIONS in STARTING-POINT hierarchy. The second returned value is the command invoked.

STARTING-POINT is a string designating the directory tree root to search.

EXPRESSIONS is a list containing test statements and logical operators. Each test statement is a list containing two elements. The first element is a string or a symbol designating a test described in man page of find(1), and the second one is an integer or a string designating the argument of this test. Logical operators include AND, OR are also available.

This function uses a command line utility find(1) for performing underlying search."
  (check-type starting-point string)
  (let* ((command (format nil "find ~A~A" starting-point (stringify-expressions expressions)))
         (files (with-output-to-string (*standard-output*)
                  (uiop:run-program command :output t)))
         (files (split-sequence:split-sequence #\Newline files))
         (files (remove-if #'(lambda (s)
                               (string= s ""))
                           files)))
    (values files command)))
