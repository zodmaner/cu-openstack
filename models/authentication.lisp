;;;; authentication.lisp

(in-package #:models)

;;; Authenticate stuffs go here.

(defun verify-user-credential (username password)
  "Verifies a user's credential and returns T if the supplied username
and password match those in the database"
  (when (not (or (null username) (null password)))
    (let ((user-password (getf (datafly:retrieve-one
                                (sxql:select :password
                                             (sxql:from :user)
                                             (sxql:where (:= :username username))))
                               :password)))
      (when (not (null user-password))
        (cl-pass:check-password password user-password)))))
