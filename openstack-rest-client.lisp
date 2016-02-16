;;;; openstack-rest-client.lisp

(in-package #:cu-openstack)

;;; "openstack-rest-client" goes here. Hacks and glory await!

(defvar *username-password* nil)

(defvar *token-and-tenant-id* nil)

(defun get-token (&key (token-tenant-id *token-and-tenant-id*))
  (getf token-tenant-id :token))

(defun get-tenant-id (&key (token-tenant-id *token-and-tenant-id*))
  (getf token-tenant-id :tenant-id))

(defun create-uri-path (&rest uri-path-components)
  (apply #'concatenate (cons 'string uri-path-components)))

(defmacro send-api-request (uri port uri-path http-method &key x-auth-token content)
  (let ((fn-args (list :method http-method)))
    (when x-auth-token
      (push `(list (cons "X-Auth-Token" ,x-auth-token)) fn-args)
      (push :additional-headers fn-args))
    (when content
      (push content fn-args)
      (push :content fn-args))
    `(let ((reply (drakma:http-request (concatenate 'string
                                                    "HTTP://" ,uri ":" ,port ,uri-path)
                                       :content-type "application/json"
                                       ,@fn-args
                                       :accept "application/json"
                                       :want-stream t)))
       (if (streamp reply)
           (if (not (null (flexi-streams:peek-byte reply nil nil nil)))
               (st-json:read-json reply)
               nil)
           reply))))

(defmacro send-api-request-with-auth (auth-token uri port uri-path http-method &key content)
  (let ((fn-args (list :x-auth-token `(get-token :token-tenant-id ,auth-token))))
    (when content
      (push content fn-args)
      (push :content fn-args))
    `(let ((*token-and-tenant-id* ,auth-token))
       (send-api-request ,uri
                         ,port
                         ,uri-path
                         ,http-method
                         ,@fn-args))))

(defun get-resource-id (resource-name resource-list)
  (cdr (assoc resource-name resource-list :test #'string=)))

(defun obtain-token-and-tenant-id (uri
                                   &key
                                     (username (getf *username-password* :username))
                                     (password (getf *username-password* :password)))
  (let ((token-struct
         (st-json:getjso
          "token"
          (st-json:getjso
           "access"
           (send-api-request uri
                             "5000"
                             "/v2.0/tokens"
                             :post
                             :content (st-json:write-json-to-string
                                       (alexandria:plist-hash-table
                                        (list "auth"
                                              (alexandria:plist-hash-table
                                               (list "tenantName" username
                                                     "passwordCredentials"
                                                     (alexandria:plist-hash-table
                                                      (list "username" username
                                                            "password" password))))))))))))
    (list :token (st-json:getjso "id" token-struct)
          :tenant-id (st-json:getjso "id"
                                     (st-json:getjso "tenant" token-struct)))))

(defun list-flavors (uri &key (token-tenant-id *token-and-tenant-id*))
  (loop
     :for jso :in (st-json:getjso
                   "flavors"
                   (send-api-request-with-auth token-tenant-id
                                               uri
                                               "8774"
                                               (create-uri-path "/v2/"
                                                                (get-tenant-id) "/flavors")
                                               :get))
     :collect (cons (st-json:getjso "name" jso)
                    (st-json:getjso "id" jso))))

(defun list-images (uri &key (token-tenant-id *token-and-tenant-id*))
  (loop
     :for jso :in (st-json:getjso
                   "images"
                   (send-api-request-with-auth token-tenant-id
                                               uri
                                               "9292"
                                               "/v2/images"
                                               :get))
     :collect (cons (st-json:getjso "name" jso)
                    (st-json:getjso "id" jso))))

(defun list-servers (uri &key (token-tenant-id *token-and-tenant-id*))
  (loop
     :for jso :in (st-json:getjso
                   "servers"
                   (send-api-request-with-auth token-tenant-id
                                               uri
                                               "8774"
                                               (create-uri-path "/v2/"
                                                                (get-tenant-id) "/servers")
                                               :get))
     :collect (cons (st-json:getjso "name" jso)
                    (st-json:getjso "id" jso))))

(defun list-servers-detail (uri &key (token-tenant-id *token-and-tenant-id*))
  (loop
     :for jso :in (st-json:getjso
                   "servers"
                   (send-api-request-with-auth token-tenant-id
                                               uri
                                               "8774"
                                               (create-uri-path "/v2/"
                                                                (get-tenant-id)
                                                                "/servers/detail")
                                               :get))
     :collect (cons (st-json:getjso "name" jso)
                    (list :id (st-json:getjso "id" jso)
                          :status (st-json:getjso "status" jso)))))

(defun create-server (uri server-name image-id flavor-id
                      &key (token-tenant-id *token-and-tenant-id*))
  (st-json:getjso
   "id"
   (st-json:getjso
    "server"
    (send-api-request-with-auth token-tenant-id
                                uri
                                "8774"
                                (create-uri-path "/v2/"
                                                 (get-tenant-id) "/servers")
                                :post
                                :content (st-json:write-json-to-string
                                          (alexandria:plist-hash-table
                                           (list "server"
                                                 (alexandria:plist-hash-table
                                                  (list "name" server-name
                                                        "imageRef" image-id
                                                        "flavorRef" flavor-id)))))))))

(defun list-floating-ips (uri &key (token-tenant-id *token-and-tenant-id*))
  (loop
     :for jso :in (st-json:getjso
                   "floating_ips"
                   (send-api-request-with-auth token-tenant-id
                                               uri
                                               "8774"
                                               (create-uri-path "/v2.1/"
                                                                (get-tenant-id) "/os-floating-ips")
                                               :get))
     :collect (list :ip (st-json:getjso "ip" jso)
                    :fixed-ip (st-json:getjso "fixed_ip" jso)
                    :pool (st-json:getjso "pool" jso))))

(defun create-floating-ip (uri &key (token-tenant-id *token-and-tenant-id*))
  (st-json:getjso
   "ip"
   (st-json:getjso
    "floating_ip"
    (send-api-request-with-auth token-tenant-id
                                uri
                                "8774"
                                (create-uri-path "/v2.1/"
                                                 (get-tenant-id) "/os-floating-ips")
                                :post
                                :content (st-json:write-json-to-string
                                          (alexandria:plist-hash-table
                                           (list "pool" "public")))))))

(defun associate-floating-ip (uri server-id floating-ip
                              &key (token-tenant-id *token-and-tenant-id*))
  (send-api-request-with-auth token-tenant-id
                              uri
                              "8774"
                              (create-uri-path "/v2.1/"
                                               (get-tenant-id) "/servers/" server-id "/action")
                              :post
                              :content (st-json:write-json-to-string
                                        (alexandria:plist-hash-table
                                         (list "addFloatingIp"
                                               (alexandria:plist-hash-table
                                                (list "address" floating-ip)))))))
