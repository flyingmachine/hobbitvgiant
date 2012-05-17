(in-package :hobbitvgiant)

(defparameter *clients* nil)

(defun json-out ()
  (with-output-to-string (*standard-output*)
    (json:encode-json (serialize hobbit))))

(defclass hvg-resource (clws:ws-resource)
  ())

(defmethod clws:resource-client-connected ((res hvg-resource) client)
  (push client *clients*)
  (clws:write-to-client-text client (json-out))
  (format t "got connection on hvg server from ~s : ~s~%" (clws:client-host client) (clws:client-port client))
  t)

(defmethod clws:resource-client-disconnected ((resource hvg-resource) client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client))

(defmethod clws:resource-received-text ((res hvg-resource) client message)
  (format t "got frame ~s from client ~s" message client)
  (setf (damage-for (damage-received (first (body-parts hobbit))) 'slice) (parse-integer message))
  (mapc (lambda (client)
          (clws:write-to-client-text client (json-out)))
        *clients*))

(defmethod clws:resource-received-binary((res hvg-resource) client message)
  (format t "got binary frame ~s from client ~s" (length message) client)
  (clws:write-to-client-binary client message))

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
