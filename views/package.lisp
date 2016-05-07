;;;; Views module's package.lisp

(defpackage #:views
  (:use #:cl)
  (:import-from #:trivial-openstack
                #:get-value)
  (:export #:login
           #:vm-management))

(uri-template:enable-uri-template-syntax)
