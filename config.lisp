;;;; config.lisp

(in-package #:cu-openstack)

;;; Configurations are defined here.

(setf *openstack-uri* "172.16.2.101")

(setf *username-password* (list :username "admin"
                                :password "1234"))
