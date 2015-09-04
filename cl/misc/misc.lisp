(in-package :cl)

(defpackage :com.liutos.misc
  (:use :cl))

(in-package :com.liutos.misc)

(defun cf-encode/rec (res ns)
  (cond ((null ns) res)
        (t (let ((n (car ns)))
             (cf-encode/rec (+ (/ 1 res) n) (cdr ns))))))

(defun cf-encode (&rest ns)
  (if (null ns)
      '()
      (let ((n (cf-encode/rec (car ns) (cdr ns))))
        (list (numerator n)
              (denominator n)))))

(defun cf-decode (n d)
  (cond ((= n 1)
         (list d))
        ((= d 1)
         (list n))
        (t (multiple-value-bind (q r)
               (truncate n d)
             (cons q (cf-decode d r))))))

(defun cf-encode-from-string (str)
  (let* ((cs (coerce str 'list))
         (ns (mapcar #'char-code cs)))
    (apply #'cf-encode ns)))

(defun cf-decode-to-string (n d)
  (reverse (coerce (mapcar #'code-char (cf-decode n d))
                   'string)))

(defun cf-encode-from-file (file)
  (with-open-file (s file
                     :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s))))
      (read-sequence bytes s)
      (apply #'cf-encode
             (coerce bytes 'list)))))

(defun cf-decode-to-file (n d file)
  (with-open-file (s file
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (let ((ns (reverse (cf-decode n d))))
      (dolist (n ns)
        (write-byte n s)))))
