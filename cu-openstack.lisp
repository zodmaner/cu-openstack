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
