(in-package :hobbitvgiant)

;; From Practical Common Lisp, ch. 9

(defun test-body-part ()
  (let* ((body-part-prototype (make-body-part-prototype 'test-head :targeting-weight 10))
         (body-part (make-body-part body-part-prototype "head")))
    (modify-damage body-part 'slice 20)
    (modify-damage body-part 'blunt 90)
    (describe-game-object body-part)))

(defun test-body ()
  (let ((body (make-body 'humanoid)))
    (modify-damage (body-part body "neck")        'slice  20)
    (modify-damage (body-part body "left eye")    'pierce 90)
    (modify-damage (body-part body "left eye")    'blunt  90)
    (modify-damage (body-part body "right thigh") 'blunt 30)
    (describe-game-object body)
    (look body)))

(defun test-attack ()
  (let ((attacker (make-body 'humanoid))
        (defender (make-body 'humanoid))
        (weapon   (select-item "dagger")))
    (attack attacker defender weapon)
    (look defender)))

(defun test-target ()
  (let ((attacker (make-body 'humanoid))
        (defender (make-body 'humanoid))
        (weapon   (select-item "dagger")))
    (body-parts-for-target-selection attacker defender weapon "head")))

(defvar giant  (make-body 'humanoid 2.3))
(defvar hobbit (make-body 'humanoid 0.8))
(defvar dagger (select-item "dagger"))

(defclass player ()
  ((player-name
    :initarg :player-name
    :accessor player-name)
   (health
    :initarg :health
    :initform 100
    :accessor health)

   (ap
    :initarg :ap
    :initform 20
    :accessor ap))
  (:metaclass observable))

(defclass game-room ()
  ((description
    :initarg :description
    :initform "It's a room"
    :accessor description)

   (latest-event
    :initarg :events
    :initform nil
    :accessor latest-event))
  (:metaclass observable))

(defvar rob (make-instance 'player :player-name "rob"))
(defvar joe (make-instance 'player :player-name "joe"))

(defvar office  (make-instance 'game-room))
(defvar kitchen (make-instance 'game-room))

(defun room-event (player event)
  (if (eql player (car event))
      (format t "Your health is now ~a~%" (third event))
      (format t "~a's health is now ~a~%" (player-name (car event)) (third event))))

(observe (rob 'health 'room-observer new old)
  (setf (latest-event kitchen) (list rob 'health new old)))

(observe (joe 'health 'room-observer new old)
  (setf (latest-event kitchen) (list joe 'health new old)))

(observe (kitchen 'latest-event 'broadcaster new)
  (room-event rob new)
  (room-event joe new))
