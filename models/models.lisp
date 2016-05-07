;;;; models.lisp

(in-package #:models)

;;; Models are defined here.

(defun initialize-db-data ()
  "Initializes tables and data in the database."
  (datafly:execute (sxql:create-table (:user :if-not-exists t)
                                      ((:username :type 'string :primary-key t)
                                       (:password :type 'string)
                                       (:realname :type 'string)
                                       (:role :type 'string)
                                       (:image :type 'string)
                                       (:flavor :type 'number))))
  (datafly:execute (sxql:insert-into :user (sxql:set= :username "smith.dh"
                                                      :password "PBKDF2$SHA256:20000$778f9d274a0557777cfd30d573b1f2f7$04676e896b92f509663823879ef3ea258659b763627dc330ccc8bb2d7a048875"
                                                      :realname "Smith Dhumbumroong"
                                                      :role "admin"
                                                      :image "cirros-0.3.4-x86_64-uec"
                                                      :flavor "1"))))

(defun init-db (&key (database :sqlite3))
  "Initializes and creates a connection to a database."
  (case database
    (:sqlite3
     (let ((db-file (merge-pathnames "db.sqlite")))
       (datafly:connect-toplevel :sqlite3 :database-name db-file)
       (initialize-db-data)))
    (otherwise
     (error "Please specify a valid database."))))

(defun retrieve-user-info (username)
  "Retrieves a user info."
  (datafly:retrieve-one (sxql:select (:realname :role :image :flavor)
                          (sxql:from :user)
                          (sxql:where (:= :username username)))))

(defun init-config (&key (path (merge-pathnames "config.json")))
  "Creates and initializes a configuration file."
  (write-config-file path))

(defun get-config-info (&key (path (merge-pathnames "config.json")))
  "Reads and returns a list containing configuration info."
  (read-config-file path))
