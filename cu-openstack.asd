;;;; cu-openstack.asd

(asdf:defsystem #:cu-openstack
  :description "Describe cu-openstack here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:cl-who
               #:drakma
               #:st-json)
  :serial t
  :components ((:file "package")
               (:file "auth")
               (:file "views")
               (:file "routes")
               (:file "cu-openstack")))

