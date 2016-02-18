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
  "Verifies the authentication token in the session cookie, and redirects a user towards
the main page via a unique uri path for each user."
  (when (string= "true" (hunchentoot:session-value :auth-token))
    (setf *token-and-tenant-id* (obtain-token-and-tenant-id *openstack-uri*))
    (hunchentoot:redirect (concatenate 'string
                                       "/vm-provisioning/users/"
                                       (hunchentoot:session-value :username)))))

(defun match-unique-user-uri-path (request)
  (string= (concatenate 'string
                        "/vm-provisioning/users/"
                        (hunchentoot:session-value :username))
           (hunchentoot:request-uri request)))

(hunchentoot:define-easy-handler (vm-provisioning :uri #'match-unique-user-uri-path) ()
  (if (string= "true" (hunchentoot:session-value :auth-token))
      (create-vm-provisioning-main-page :vm-name (hunchentoot:session-value :vm-name)
                                        :vm-status (get-resource-id
                                                    (concatenate 'string
                                                                 (hunchentoot:session-value :role)
                                                                 "-"
                                                                 (hunchentoot:session-value :username))
                                                    (list-servers-detail *openstack-uri*))
                                        :page-uri (hunchentoot:request-uri*)
                                        :realname (hunchentoot:session-value :realname))
      (hunchentoot:redirect "/login")))

(hunchentoot:define-easy-handler (launch-new-vm-instant :uri "/launch-a-new-instant") ()
  (let ((instant-name (concatenate 'string
                                   (hunchentoot:session-value :role)
                                   "-"
                                   (hunchentoot:session-value :username))))
    (setf *token-and-tenant-id* (obtain-token-and-tenant-id *openstack-uri*))
    (create-server *openstack-uri*
                   instant-name
                   (get-resource-id (hunchentoot:session-value :vm-name) (list-images *openstack-uri*))
                   (get-resource-id "m1.tiny" (list-flavors *openstack-uri*)))
    (loop
       :while (string/= "ACTIVE" (getf (get-resource-id instant-name (list-servers-detail *openstack-uri*))
                                       :status))
       :do
       (sleep 1))
    (let ((floating-ip (create-floating-ip *openstack-uri*)))
      (associate-floating-ip *openstack-uri*
                             (get-resource-id instant-name (list-servers *openstack-uri*))
                             floating-ip))
    (hunchentoot:redirect (concatenate 'string
                                       "/vm-provisioning/users/"
                                       (hunchentoot:session-value :username)))))

(hunchentoot:define-easy-handler (clear-session-and-redirect :uri "/clear-and-logout") ()
  (setf (hunchentoot:session-value :authenticated) "false")
  (hunchentoot:remove-session hunchentoot:*session*)
  (hunchentoot:redirect "/login"))
