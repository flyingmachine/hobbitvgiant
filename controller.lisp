(in-package :hobbitvgiant)
(defparameter *clients* nil)
(defparameter *players* (make-hash-table))
(defparameter *room* nil)

(defun start-game ()
  (start-clws)
  (setf *room* (make-instance 'game-room
                              :name "A green meadow"
                              :description "An idyllic meadow spreads before you")))

(defun client-player (client)
  (gethash client *players*))

(defmethod clws:resource-client-connected ((res hvg-resource) client)
  (let ((player (make-instance 'player
                               :name (clws:client-query-string client)
                               :body (make-body 'humanoid))))
    (move-to-room (body player) *room*)
    (setf (gethash client *players*) player)
    ;; add observer
    (push (lambda (player channel event)
            (clws:write-to-client-text client (json-out event)))
          (notification-handlers player))

    ;; send this player to all clients, includ
    (mapc (lambda (other-client)
            (clws:write-to-client-text other-client (json-out (make-hash-from-pairsr 'add (list (list (make-hash-from-pairsr 'body (serialize (body player)))))))))
          (append1 *clients* client))

    ;; send all other players to client
    ;; this must come after the player's character is initially sent,
    ;; as the client code assumes that the first player received
    ;; belongs to you
    (clws:write-to-client-text client (json-out (make-hash-from-pairsr 'add (list (mappend (lambda (player) (make-hash-from-pairsr 'body (serialize (body player)))) (hash-values *players*))))))


    )
  (push client *clients*)
  t)

(defmethod clws:resource-client-disconnected ((resource hvg-resource) client)
  ;; TODO delete hash element
  (remhash client *players*)
  (setf *clients* (remove client *clients*)))

(defmethod clws:resource-received-text ((res hvg-resource) client message)
  (setf (damage-for (damage-received (first (body-parts (body (client-player client))))) 'slice) (parse-integer message))
  (mapc (lambda (client)
          (clws:write-to-client-text client (json-out (client-player client))))
        *clients*))
