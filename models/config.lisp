;;;; config.lisp

(in-package #:models)

;;; Configuration file is managed here.

(defun write-config-file (path)
  "Writes a configuration file to a path."
  (with-open-file (out path :direction :output :if-does-not-exist :create :if-exists nil)
    (st-json:write-json
     (alexandria:plist-hash-table
      (list "keystone-host" "127.0.0.1"
            "username" "admin"
            "password" "swordfish"))
     out)))

(defun read-config-file (path)
  "Reads a configuration file from a path."
  (with-open-file (in path :direction :input :if-does-not-exist :error)
    (let* ((config-jso (st-json:read-json in))
           (keystone-host (st-json:getjso "keystone-host" config-jso))
           (username (st-json:getjso "username" config-jso))
           (password (st-json:getjso "password" config-jso)))
      (list keystone-host username password))))
