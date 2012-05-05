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
      (register-player game-room (car new)))))

(defun register-player (room player)
  (observe (room 'latest-event (symb-up 'player- (name player)) new)
    (notify-player player 'room new))
  
  (observe ((body player) 'stamina (symb 'room (id room)) new)
    (setf (latest-event room) (list player 'stamina new)))

  (observe-each ((body-parts (body player)) 'damage-received (symb 'room (id room)) new)
    (setf (latest-event room) (list player 'dam-received new))))

;; requires 'remove observer' fun or macro
(defun deregister-player (room player)
  (remove-observer room 'latest-event (symb-up 'player- (name player)))
  ;; TODO remove all other observers
  )
