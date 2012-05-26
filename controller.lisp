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

(defun serialize-add-player (player key)
  (json-out (make-hash-from-pairsr 'add (list (list (make-hash-from-pairsr key (serialize (body player))))))))

(defmethod clws:resource-client-connected ((res hvg-resource) client)
  (let ((player (make-instance 'player
                               :name (clws:client-query-string client)
                               :body (make-body 'humanoid))))

    (move-to-room (body player) *room*)
    ;; add observer
    (push (lambda (player channel event)
            (clws:write-to-client-text client (json-out event)))
          (notification-handlers player))

    ;; send this player to all other clients
    (mapc (lambda (other-client)
           (clws:write-to-client-text other-client (serialize-add-player player 'player)))
          *clients*)

    (clws:write-to-client-text client (serialize-add-player player 'current-player))

    ;; send all other players to client
    ;; this must come after the player's character is initially sent,
    ;; as the client code assumes that the first player received
    ;; belongs to you
    (clws:write-to-client-text client (json-out (make-hash-from-pairsr 'add (list (mappend (lambda (player) (list (make-hash-from-pairsr 'player (serialize (body player))))) (hash-values *players*))))))
    
    (setf (gethash client *players*) player))
  
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
