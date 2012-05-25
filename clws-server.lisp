(in-package :hobbitvgiant)

(defclass hvg-resource (clws:ws-resource)
  ())

(defun start-clws ()
  (bordeaux-threads:make-thread (lambda ()
                                  (clws:run-server 12345))
                                :name "hvg server")
  (clws:register-global-resource "/game"
                                 (make-instance 'hvg-resource)
                                 (clws:origin-prefix "http://127.0.0.1:4567" "http://localhost:4567"))
  (bordeaux-threads:make-thread (lambda ()
                                  (clws:run-resource-listener
                                   (clws:find-global-resource "/game")))
                                :name "resource listener for /game"))
