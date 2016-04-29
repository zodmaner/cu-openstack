;;;; models.lisp

(in-package #:models)

;;; Models are defined here.

(defun initialize-db-data ()
  "Initializes tables and data in the database."
  (datafly:execute (sxql:create-table (:user :if-not-exists t)
                                      ((:username :type 'string :primary-key t)
                                       (:password :type 'string)
                                       (:realname :type 'string)
                                       (:role :type 'string))))
  (datafly:execute (sxql:insert-into :user (sxql:set= :username "admin"
                                                      :password "PBKDF2$SHA256:20000$778f9d274a0557777cfd30d573b1f2f7$04676e896b92f509663823879ef3ea258659b763627dc330ccc8bb2d7a048875"
                                                      :realname "Smith Dhumbumroong"
                                                      :role "admin"))))

(defun init-db (&key (database :sqlite3))
  "Initializes and creates a connection to a database."
  (cond ((string= database :sqlite3)
         (let ((db-file (merge-pathnames "db.sqlite")))
           (datafly:connect-toplevel :sqlite3 :database-name db-file)
           (initialize-db-data)))
        (t
         (error "Please specify a valid database."))))
