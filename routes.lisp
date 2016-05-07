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
           (setf (hunchentoot:session-value :vm-name)
                 (format nil "~A-~A"
                         (getf (models:retrieve-user-info username) :role)
                         username))
           (hunchentoot:redirect #U/users/{username}/vm-management))
         (views:login)))
    (:get
     (views:login))))

(hunchentoot:define-easy-handler
    (vm-management-page
     :uri #'(lambda (request)
              (uri-template:uri-template-bind
                  (#U/users/{username}/vm-management{query-fragment})
                  (hunchentoot:request-uri request)
                (string= username (hunchentoot:session-value :username))))) ()
  "Virtual machine management page."
  (if (string/= "" (hunchentoot:session-value :username))
      (let ((username (hunchentoot:session-value :username))
            (vm-name (hunchentoot:session-value :vm-name))
            (user-info (models:retrieve-user-info
                        (hunchentoot:session-value :username))))
        (case (hunchentoot:request-method*)
          (:get
           (models:os-authenticate "localhost" "dummy" "swordfish")
           
           (views:vm-management :username username
                                :vm-name vm-name
                                :vm-status (models:os-vm-instant-status vm-name)
                                :vm-image (getf user-info :image)
                                :vm-flavor (models:os-list-flavor-details (getf user-info :flavor))
                                :vm-floating-ip (models:os-get-vm-instant-floating-ip vm-name)
                                :page-uri (hunchentoot:request-uri*)
                                :realname (getf user-info :realname)))
          (:post
           (models:os-launch-vm-instant vm-name
                                        (models:os-get-image-id (getf user-info :image))
                                        (format nil "~A" (getf user-info :flavor)))
           (hunchentoot:redirect #U/users/{username}/vm-management))))
      (hunchentoot:redirect "/login")))

(hunchentoot:define-easy-handler
    (clear-session-and-redirect :uri "/clear-and-logout") ()
  "Clears session information and logout a user."
  (hunchentoot:remove-session hunchentoot:*session*)
  (hunchentoot:redirect "/login"))
