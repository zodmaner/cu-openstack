;;;; auth.lisp

(in-package #:cu-openstack)

;;; Authenticate stuffs go here.

(defparameter *user-list* (list (cons "5571432021"
                                (list :password "1234"
                                      :role "student"
                                      :vm-name "cirros-0.3.4-x86_64-uec"
                                      :realname "Smith Dhumbumroong")))
  "An ad-hoc list of user credentials.")

(defun get-user-credential (username user-list)
  "Given a list of user credentials, returns a credential of a user with
username."
  (cdr (assoc username user-list :test #'string=)))

(defun get-password (user-credential)
  "Given a user credential, returns a password of that user."
  (getf user-credential :password))

(defun get-realname (user-credential)
  "Given a user credential, returns the real name of that user."
  (getf user-credential :realname))

(defun get-vm-name (user-credential)
  "Given a user credential, returns the VM name that the user is allowed to use."
  (getf user-credential :vm-name))

(defun get-role (user-credential)
  "Given a user credential, returns the role of a user."
  (getf user-credential :role))

(defun verify-user-credential (username password &key (user-list *user-list*))
  (let ((user-credential (get-user-credential username user-list)))
    (when (and (not (null user-credential))
               (string= password (get-password user-credential)))
      user-credential)))

(defun authenticate-set-session-and-redirect (username password
                                              success-target reject-target)
  (let ((user-credential (verify-user-credential username password)))
    (cond (user-credential
           (hunchentoot:start-session)
           (setf (hunchentoot:session-value :username) username)
           (setf (hunchentoot:session-value :realname) (get-realname user-credential))
           (setf (hunchentoot:session-value :role) (get-role user-credential))
           (setf (hunchentoot:session-value :vm-name) (get-vm-name user-credential))
           (setf (hunchentoot:session-value :auth-token) "true")
           (hunchentoot:redirect success-target))
          (t
           (hunchentoot:redirect reject-target)))))
