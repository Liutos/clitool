;;;; genji-task-manager.lisp

(in-package #:genji-task-manager)

;;; "genji-task-manager" goes here. Hacks and glory await!

(defun foobar (request)
  (declare (ignorable request))
  (eloquent.mvc.controller:json-body-bind
      ((uri "uri"))
      request
    (redis:with-connection ()
      (red:set uri uri))
    (eloquent.mvc.response:respond
     uri)))
