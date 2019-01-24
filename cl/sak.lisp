;;; SAK means Swiss army knife
(in-package :cl-user)

(defpackage #:com.liutos.sak
  (:use #:cl))

(in-package #:com.liutos.sak)

(defmacro while (test &body body)
  "C语言风格的while循环"
  `(loop
      :while ,test
      :do (progn ,@body)))
