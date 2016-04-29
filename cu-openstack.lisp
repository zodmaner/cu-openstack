;;;; cu-openstack.lisp

(in-package #:cu-openstack)

;;; "cu-openstack" goes here. Hacks and glory await!

(defun start-easy-server (port)
  "Starts the server by starting the easy-acceptor and returns a stop-server
closure function that can be used to stop the active acceptor and shutdown
the server."
  (declare (type fixnum port))
  (let ((active-acceptor (hunchentoot:start
                          (make-instance 'hunchentoot:easy-acceptor
                                         :port port
                                         :document-root (merge-pathnames
                                                         "static")))))
    (defun stop-easy-server ()
      "A closure function that closes over an active acceptor started by
the start-server function and can be used to stop the said active acceptor
and shutdown the server."
      (hunchentoot:stop active-acceptor))))

(defun main ()
  "Initializes the database and starts the server using the start-easy-server
function. This is intended to be used as the toplevel function when creating a
Lisp image, and the join-thread function call is needed to prevent the image
from exiting right after starting the server."
  (let* ((port-argv (second sb-ext:*posix-argv*))
         (port (if (null port-argv)
                   8080
                   (parse-integer port-argv))))
    (models:init-db)
    (start-easy-server port)
    (bt:join-thread (find-if #'(lambda (thread)
                                 (string= (bt:thread-name thread)
                                          (format nil "hunchentoot-listener-*:~A"
                                                  port)))
                             (bt:all-threads)))))
