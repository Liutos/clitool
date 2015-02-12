(in-package :common-lisp)

(defpackage :liutos.cli.uniq
  (:use :common-lisp)
  (:export :uniq))

(in-package :liutos.cli.uniq)

(defun uniq-read (stream &key (skip-chars 0) (checkchars nil) (ignore-casep nil)
                         &allow-other-keys)
  (let* ((line (read-line stream nil)))
    (unless line
      (return-from uniq-read line))
    (let* ((len (or checkchars (- (length line) skip-chars -1)))
           (line (subseq line skip-chars len)))
      (if ignore-casep
          (string-downcase line)
          line))))

(defun matchp (line status repeatedp)
  (if repeatedp
      (> (gethash line status) 1)
      t))

(defun uniq-display (lines status
                     &key
                       (countp nil)
                       (repeatedp nil)
                     &allow-other-keys)
  (dolist (line lines)
    (when (matchp line status repeatedp)
      (when countp
        (format t "~D " (gethash line status)))
      (format t "~A~%" line))))

;;; 这个实现的uniq是错误的，因为输出的时候按照skip-chars和check-chars的设置将原本的一行内容给截断了，而真正的uniq的实现是并没有将输出内容截断的
(defun uniq (file &rest args
             &key
               (countp nil)
               (repeatedp nil)
               (skip-chars 0)
               (checkchars nil)
               (ignore-casep nil))
  "Report or omit repeated lines."
  (declare (ignorable countp repeatedp))
  (declare (ignorable skip-chars checkchars ignore-casep))
  (with-open-file (s file)
    (let ((tbl (make-hash-table :test #'equal))
          (results '()))
      (loop
         :for line = (apply #'uniq-read s args)
         :while line
         :if (null (gethash line tbl))
         :do (progn
               (setf (gethash line tbl) 1)
               (push line results))
         :else :do (incf (gethash line tbl))
         :finally (setf results (nreverse results)))
      (apply #'uniq-display results tbl args))))
