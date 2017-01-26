;;;; genji-task-manager.asd

(asdf:defsystem #:genji-task-manager
  :description "Describe genji-task-manager here"
  :author "liutos"
  :license "Specify license here"
  :depends-on (#:cl-httpsqs
               #:cl-redis
               #:eloquent-mvc)
  :serial t
  :components ((:file "package")
               (:file "genji-task-manager")))

