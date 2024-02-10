;;; 以下代码改编自[这里](https://www.runoob.com/design-pattern/proxy-pattern.html)。
;;; 实际上实现的是【懒加载】的效果。
(in-package :cl-user)

(defpackage #:com.liutos.clitool.proxy-demo
  (:use #:cl))

(in-package #:com.liutos.clitool.proxy-demo)

(defgeneric display (image))

(defclass real-image ()
  ((file-name
    :initarg :file-name
    :reader real-image-file-name)))

(defmethod initialize-instance :after ((instance real-image) &rest initargs &key)
  (declare (ignorable initargs))
  (format t "Loading ~A~%" (real-image-file-name instance)))

(defmethod display ((image real-image))
  (format t "Displaying ~A~%" (real-image-file-name image)))

(defclass proxy-image ()
  ((file-name
    :initarg :file-name
    :reader real-image-file-name)
   (real-image
    :accessor proxy-image-real-image
    :initform nil)))

(defmethod display ((image proxy-image))
  (unless (proxy-image-real-image image)
    (setf (proxy-image-real-image image)
          (make-instance 'real-image :file-name (real-image-file-name image))))

  (display (proxy-image-real-image image)))

(defun main ()
  (let ((image (make-instance 'proxy-image :file-name "test_10mb.jpg")))
    (display image)
    (format t "~%")
    (display image)))
