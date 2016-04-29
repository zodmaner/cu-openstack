;;;; Views module's package.lisp

(defpackage #:views
  (:use #:cl)
  (:export #:login
           #:vm-provisioning))

(uri-template:enable-uri-template-syntax)
