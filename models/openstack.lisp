;;;; openstack.lisp

(in-package #:models)

;;; OpenStack interface is defined here.

(defun os-authenticate (keystone-host username password)
  "Authenticates with OpenStack Keystone (identity) server."
  (authenticate keystone-host username password))

(defun os-vm-instant-status (instant-name)
  "Returns the current status of a virtual machine instant with the
  specified name."
  (get-value (list-servers-details) instant-name "status"))

(defun os-list-flavor-details (flavor-id)
  "Lists details of a flavor of a given ID."
  (list-flavor-details flavor-id))

(defun os-get-vm-instant-floating-ip (instant-name)
  "Retrieves the floating IP of a virtual machine instant."
  (get-value (list-servers-details) instant-name "addresses" "floating"))

(defun os-get-image-id (image-name)
  "Retrieves the image ID given image name."
  (get-value (list-images) image-name))

(defun os-launch-vm-instant (instant-name image-id flavor-id)
  "Launches a new virtual machine instant."
  (create-server instant-name image-id flavor-id)
  (loop
     :while (string/= "ACTIVE" (os-vm-instant-status instant-name))
     :do (sleep 1))
  (let ((floating-ip (create-floating-ip)))
    (associate-floating-ip (get-value (list-servers) instant-name) floating-ip)))
