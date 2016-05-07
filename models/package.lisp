;;;; Models module's package.lisp

(defpackage #:models
  (:use #:cl #:trivial-openstack)
  (:export #:init-db
           #:init-config
           #:get-config-info
           #:retrieve-user-info
           #:verify-user-credential
           #:os-authenticate
           #:os-vm-instant-status
           #:os-list-flavor-details
           #:os-get-vm-instant-floating-ip
           #:os-get-image-id
           #:os-launch-vm-instant))
