;;;; views.lisp

(in-package #:cu-openstack)

;;; Views are defined here.

(setf (cl-who:html-mode) :html5)

;;; Template definitions

(defmacro with-base-html-template ((output-stream &rest html-head) &body body)
  "Generates a web page with the enclosed HTML head and body using the base HTML template
and writes the result out to the stream output-stream."
  `(cl-who:with-html-output (,output-stream nil :indent t)
     (:html
      (:head
       ,@html-head)
      (:body
       ,@body))))

(defmacro with-default-html-template ((output-stream page-title path-to-css) &body body)
  "Generates a web page using the default HTML template along with the enclosed body, with
page-title as the page's title and path-to-css as a path to the page's CSS."
  `(cl-who:with-html-output (,output-stream nil :indent t)
     (with-base-html-template (,output-stream
                               (:title (cl-who:str ,page-title))
                               (:meta :charset "UTF-8")
                               (:link :rel "icon"
                                      :href "/static/img/cu-eng-logo.ico"
                                      :type "image/x-icon")
                               (:link :rel "stylesheet" :href (cl-who:str ,path-to-css)))
       ,@body)))

;;; View definitions

(defun create-login-page ()
  "Creates the log-in page."
  (cl-who:with-html-output-to-string (output-string nil :prologue t :indent t)
    (with-default-html-template (output-string "Welcome to CU OpenStack System"
                                               "/static/css/login-page.css")
      (:div :id "login-box-container"
            (:div :id "login-box-logo"
                  (:img :src "/static/img/cu-eng-logo.png"))
            (:div :id "login-box-header"
                  (:p "Welcome to CU OpenStack System"))
            (:div :id "login-box-body"
                  (:form :action "/auth" :method "post"
                         (:p "Username:" (:br)
                             (:input :type "text" :name "username"))
                         (:br)
                         (:p "Password:" (:br)
                             (:input :type "password" :name "password"))
                         (:br)
                         (:p (:input :type "submit" :value "Log-in")))))
      (:div :id "watermark"
            (:p
             "Copyright "
             (multiple-value-bind (second minute hour date month year) (get-decoded-time)
               (declare (ignore second minute hour date month))
               (cl-who:fmt "~A, " year))
              "Smith Dhumbumroong")))))

(defun create-main-page (&key (page-uri "/") (realname "nil"))
  (cl-who:with-html-output-to-string (output-string nil :prologue t :indent t)
    (with-default-html-template (output-string "CU OpenStack System"
                                               "/static/css/main-page.css")
      (:header
       (:div :id "header-left"
             (:div :class "header-elt"
                   (:img :src "/static/img/cu-eng-logo-with-shadow-00.png"))
             (:div :class "header-elt"
                   (:p "CU OpenStack System"))
             (:div :class "header-selected-elt"
                   (:a :href (cl-who:str page-uri) "Virtual Machine Provisioning")))
       (:div :id "header-right"
             (:div :class "header-elt"
                   (:p "User: " (cl-who:str realname)))
             (:div :class "header-interactive-elt"
                   (:a :href "/clear-and-logout" "Log-out"))))
      (:div :id "container"
            (:div :id "main-content"
                  (:h1 "Your Virtual Machine Status")
                  (:hr)
                  (:br)
                  (:p "Currently, you don't have any active virtual machine.")
                  (:br)
                  (:p "Based on your credential, you have access to the following virtual machine:" )
                  (:br)
                  (:div :class "vm-spec-container"
                        (:h4 "CirrOS")
                        (:hr)
                        (:br)
                        (:table
                         (:tr
                          (:td (:b "CPU:"))
                          (:td "1 vCPU"))
                         (:tr
                          (:td (:b "Memory:"))
                          (:td "1 GB"))
                         (:tr
                          (:td (:b "HDD:"))
                          (:td "1 GB"))))
                  (:br)
                  (:div :class "link-container"
                        (:a :href "/main" "Launch a new instant"))))
      (:div :id "watermark"
            (:p
             "Copyright "
             (multiple-value-bind (second minute hour date month year) (get-decoded-time)
               (declare (ignore second minute hour date month))
               (cl-who:fmt "~A, " year))
             "Smith Dhumbumroong")))))
