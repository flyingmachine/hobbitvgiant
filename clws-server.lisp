(in-package :hobbitvgiant)

(defparameter *clients* nil)

(defun json-out (player)
  (with-output-to-string (*standard-output*)
    (json:encode-json (serialize (body player)))))

(defclass hvg-resource (clws:ws-resource)
  ())

(defmethod clws:resource-client-connected ((res hvg-resource) client)
  (push client *clients*)
  (mapc (lambda (client)
          (clws:write-to-client-text client (json-out (add-player client "bill"))))
        *clients*)
  t)

(defmethod clws:resource-client-disconnected ((resource hvg-resource) client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client))

(defmethod clws:resource-received-text ((res hvg-resource) client message)
  (setf (damage-for (damage-received (first (body-parts (body (client-player client))))) 'slice) (parse-integer message))
  (mapc (lambda (client)
          (clws:write-to-client-text client (json-out (client-player client))))
        *clients*))

(defun start-clws ()
  (start-game)
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
