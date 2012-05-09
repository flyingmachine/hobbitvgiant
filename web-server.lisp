(in-package :hobbitvgiant)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (attack :uri "/attack") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (*standard-output*)
    (test-attack)))

