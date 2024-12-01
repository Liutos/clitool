(in-package :cl-user)

(defpackage :cl-accounting
  (:use :cl
        :cl-accounting.entity
        :cl-accounting.repo)
  (:export :main))

(in-package :cl-accounting)

(defun main ()
  (cl-accounting.web:init)
  (cl-accounting.web:start))
