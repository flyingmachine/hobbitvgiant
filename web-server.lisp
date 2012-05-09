(in-package :hobbitvgiant)

(defvar *acceptor* nil)
;;(hunchentoot:start *acceptor*)

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :hobbitvgiant path)))

(defun start-server (&optional (port 4242))
  (if *acceptor*
      (hunchentoot:stop *acceptor*))
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port port
                                  :document-root (resource-path "www")))
  (hunchentoot:start *acceptor*))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (attack :uri "/attack") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (*standard-output*)
    (test-attack)))

;;(defun resource-path (path)
;;  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
;;  (truename (asdf:system-relative-pathname :hobbitvgiant path)))
;;
;;(defvar *acceptor* nil "the hunchentoot acceptor")
;;(defun start-server (&optional (port 8888))
;;  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
;;  ;; make a folder dispatcher the last item of the dispatch table
;;  ;; if a request doesn't match anything else, try it from the filesystem
;;  (setf (alexandria:last-elt hunchentoot:*dispatch-table*)
;;	(hunchentoot:create-folder-dispatcher-and-handler "/" (resource-path "www")))
;;  (hunchentoot:start *acceptor*))
