;;;; routes.lisp

(in-package #:cu-openstack)

;;; Route definitions go here.

(hunchentoot:define-easy-handler
    (redirect-to-login :uri "/") ()
  "Redirects user from the base URL to the login page."
  (hunchentoot:redirect "/login"))

(hunchentoot:define-easy-handler
    (login-page :uri "/login") ((username :request-type :post)
                                (password :request-type :post))
  "Login page that handles user authentication."
  (case (hunchentoot:request-method*)
    (:post
     (if (models:verify-user-credential username password)
         (progn
           (hunchentoot:start-session)
           (setf (hunchentoot:session-value :username) username)
           (hunchentoot:redirect #U/users/{username}/vm-provisioning))
         (views:login)))
    (:get
     (views:login))))

(hunchentoot:define-easy-handler
    (vm-provisioning
     :uri #'(lambda (request)
              (uri-template:uri-template-bind
                  (#U/users/{username}/vm-provisioning)
                  (hunchentoot:request-uri request)
                (string= username (hunchentoot:session-value :username))))) ()
  "Virtual machine provisional page."
  (views:vm-provisioning :vm-name (hunchentoot:session-value :vm-name)
                         :vm-status (t-os:cdr-assoc
                                     (format nil "~A-~A"
                                             (hunchentoot:session-value :role)
                                             (hunchentoot:session-value :username))
                                     (t-os:list-servers-detail))
                         :page-uri (hunchentoot:request-uri*)
                         :realname (hunchentoot:session-value :realname))
  (hunchentoot:redirect "/login"))

(hunchentoot:define-easy-handler (launch-new-vm-instant :uri "/launch-a-new-instant") ()
  (let ((instant-name (format nil "~A-~A"
                              (hunchentoot:session-value :role)
                              (hunchentoot:session-value :username))))
    (t-os:create-server instant-name
                        (t-os:cdr-assoc (hunchentoot:session-value :vm-name)
                                        (t-os:list-images))
                        (t-os:cdr-assoc "m1.tiny"
                                        (t-os:list-flavors)))
    (loop
       :while (string/= "ACTIVE"
                        (t-os:cdr-assoc "status"
                                        (t-os:cdr-assoc instant-name
                                                        (t-os:list-servers-detail))))
       :do
       (sleep 1))
    (let ((floating-ip (t-os:create-floating-ip)))
      (t-os:associate-floating-ip (t-os:cdr-assoc "instant-name" (t-os:list-servers))
                                  floating-ip))
    (hunchentoot:redirect (concatenate 'string
                                       "/vm-provisioning/users/"
                                       (hunchentoot:session-value :username)))))

(hunchentoot:define-easy-handler
    (clear-session-and-redirect :uri "/clear-and-logout") ()
  "Clears session information and logout a user."
  (hunchentoot:remove-session hunchentoot:*session*)
  (hunchentoot:redirect "/login"))
