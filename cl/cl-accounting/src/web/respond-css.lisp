(in-package #:cl-accounting.web)

(defun make-static-file-handler (file-path
                                 &key content-type)
  "生成一个 Hunchentoot 的路由规则的 handler，用于返回静态文件 FILE-PATH 的内容。"
  (lambda ()
    (when content-type
      (setf (hunchentoot:content-type*) content-type))
    
    (uiop:read-file-string file-path)))

(defun respond-amis-css-file ()
  "根据请求的 URL 来返回一个 AMIS 的 .css 文件。"
  (let* ((amis-css-dir "/app/src/web/amis")
         (css-file (format nil "~A~A" amis-css-dir (hunchentoot:request-uri*))))
    (setf (hunchentoot:content-type*) "Content-Type: text/css")
    (uiop:read-file-string css-file)))

(defun respond-amis-js-file ()
  "根据请求的 URL 来返回一个 AMIS 的 .js 文件。"
  (let* ((amis-css-dir "/app/src/web/amis")
         (css-file (format nil "~A~A" amis-css-dir (hunchentoot:request-uri*))))
    (setf (hunchentoot:content-type*) "Content-Type: text/javascript")
    (uiop:read-file-string css-file)))
