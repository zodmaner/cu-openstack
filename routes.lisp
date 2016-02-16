;;;; routes.lisp

(in-package #:cu-openstack)

;;; Route definitions go here.

(hunchentoot:define-easy-handler (redirect-to-login :uri "/") ()
  "Redirects user from the base URL to the login page."
  (hunchentoot:redirect "/login"))

(hunchentoot:define-easy-handler (user-login :uri "/login"
                                             :default-request-type :get) ()
  "Handles user authentication."
  (create-login-page))

(hunchentoot:define-easy-handler (authen-user :uri "/auth"
                                              :default-request-type :post)
    ((username :request-type :post)
     (password :request-type :post))
  "Authenticates a user using information from post parameters."
  (authenticate-set-session-and-redirect username
                                         password
                                         "/verify-and-redirect"
                                         "/login"))

(hunchentoot:define-easy-handler (verify-auth-token-and-redirect :uri "/verify-and-redirect") ()
  (when (string= "true" (hunchentoot:session-value :auth-token))
    (hunchentoot:redirect (concatenate 'string
                                       "/vm-provisioning/users/"
                                       (hunchentoot:session-value :username)))))

(defun construct-unique-user-uri-path (request)
  (string= (concatenate 'string
                        "/vm-provisioning/users/"
                        (hunchentoot:session-value :username))
           (hunchentoot:request-uri request)))

(hunchentoot:define-easy-handler (main-page :uri #'construct-unique-user-uri-path) ()
  (if (string= "true" (hunchentoot:session-value :auth-token))
      (create-main-page :page-uri (hunchentoot:request-uri*)
                        :realname (hunchentoot:session-value :realname))
      (hunchentoot:redirect "/login")))

(hunchentoot:define-easy-handler (clear-session-and-redirect :uri "/clear-and-logout") ()
  (setf (hunchentoot:session-value :authenticated) "false")
  (hunchentoot:remove-session hunchentoot:*session*)
  (hunchentoot:redirect "/login"))
