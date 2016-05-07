;;;; cu-openstack.asd

(asdf:defsystem #:cu-openstack
  :description "A simple OpenStack web application/middleware."
  :author "Smith Dhumbumroong <zodmaner@gmail.com>"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-pass
               #:datafly
               #:uri-template
               #:trivial-openstack)
  :serial t
  :components ((:module "models"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "models")
                         (:file "authentication")
                         (:file "openstack")))
               (:module "views"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "views")))
               (:file "package")
               (:file "routes")
               (:file "cu-openstack")))

