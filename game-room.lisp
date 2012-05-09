(in-package :hobbitvgiant)

(defclass game-room (game-object)
  ((description
    :initarg :description
    :initform "It's a room"
    :accessor description)

   (bodies
    :documentation "List of bodies in the room"
    :initarg :bodies
    :initform nil
    :accessor bodies)

   (latest-event
    :initarg :events
    :initform nil
    :accessor latest-event))
  (:metaclass observable))

;; TODO make this refer to bodies, and then from bodies figure out
;; whether there's a player
(defmethod initialize-instance :after ((game-room game-room) &key)
  (observe (game-room 'latest-event 'notify-bodies new old)
    (mapc (lambda (player) (notify-player player 'room new))
          (remove-if #'null (mapcar #'player (bodies game-room))))))

(defun move-to-room (body new-room)
  (if-let (old-room (game-room body))
    (setf (bodies old-room) (remove body (bodies old-room))))
  (pushnew body (bodies new-room))
  (setf (game-room body) new-room))
