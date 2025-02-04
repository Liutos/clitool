(defsystem "cl-accounting"
  :description "记账"
  :version "0.1.0"
  :license "BSD"
  :pathname "src/"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-dbi"
               "cl-ppcre"
               "hunchentoot"
               "local-time"
               "swank"
               "yason"
               "uiop")
  :components ((:module "app"
                        :components ((:file "condition"
                                            :depends-on ("package"))
                                     (:file "create-account"
                                      :depends-on ("condition" "package"))
                                     (:file "create-transfer"
                                            :depends-on ("package"))
                                     (:file "delete-account"
                                            :depends-on ("condition" "package"))
                                     (:file "list-account"
                                            :depends-on ("package"))
                                     (:file "list-transfer"
                                            :depends-on ("package"))
                                     (:file "package")
                                     (:file "tree-account"
                                            :depends-on ("package"))
                                     (:file "unit-of-work"
                                      :depends-on ("package")))
                        :depends-on ("entity"))
               (:module "entity"
                        :components ((:file "package")
                                     (:file "account")
                                     (:file "transfer"))
                        :serial t)
               (:module "infra"
                        :components ((:file "connection")))
               (:file "main"
                :depends-on ("app"
                             "entity"
                             "repo"
                             "web"))
               (:module "repo"
                        :components ((:file "package")
                                     (:file "account")
                                     (:file "transfer")
                                     (:file "unit-of-work"))
                        :depends-on ("app" "entity")
                        :serial t)
               (:module "web"
                        :components ((:file "create-account"
                                            :depends-on ("package"))
                                     (:file "create-transfer"
                                            :depends-on ("package"))
                                     (:file "delete-account"
                                            :depends-on ("package"))
                                     (:file "list-account"
                                            :depends-on ("package"))
                                     (:file "list-transfer"
                                            :depends-on ("package"))
                                     (:file "package")
                                     (:file "respond-css"
                                            :depends-on ("package"))
                                     (:file "server"
                                            :depends-on ("package"
                                                         "tree-account"))
                                     (:file "tree-account"
                                            :depends-on ("package")))
                        :depends-on ("app" "infra"))))
