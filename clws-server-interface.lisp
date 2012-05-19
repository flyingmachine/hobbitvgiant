(in-package :hobbitvgiant)
(defparameter *players* (make-hash-table))
(defparameter *room* nil)
(defun start-game ()
  (setf *room* (make-instance 'room
                              :name "A green meadow"
                              :description "An idyllic meadow spreads before you")))

(defun add-player (client player-name)
  (let ((player (make-instance 'player
                               :name player-name
                               :body (make-body 'humanoid))))
    (move-to-room (body player) *room*)
    (setf (gethash client *players*) player)))

(defun client-player (client)
  (gethash client *players*))
