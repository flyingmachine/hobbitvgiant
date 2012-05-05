(in-package :hobbitvgiant)

(defclass game-room (game-object)
  ((description
    :initarg :description
    :initform "It's a room"
    :accessor description)

   (monsters
    :documentation "List of monsters in the room"
    :initarg :monsters
    :initform nil
    :accessor monsters)

   (players
    :documentation "List of players in the room"
    :initarg :players
    :initform nil
    :accessor players)

   (latest-event
    :initarg :events
    :initform nil
    :accessor latest-event))
  (:metaclass observable))

;; TODO make this refer to bodies, and then from bodies figure out
;; whether there's a player
(defmethod initialize-instance :after ((game-room game-room) &key)
  (observe (game-room 'players 'player-observer new old)
    (if-let (removed (set-difference old new))
      (deregister-player game-room (first removed))
      (register-player game-room (first new)))))

(defun register-player (room player)
  (observe (room 'latest-event (symb-up 'player- (name player)) new)
    (notify-player rob 'room new))
  
  (observe ((body player) 'health (symb 'room (id room)) new)
    (setf (latest-event room) (player 'health new))))

;; requires 'remove observer' fun or macro
(defun deregister-player (room player)
  )
