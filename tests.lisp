(in-package :hobbitvgiant)

;; From Practical Common Lisp, ch. 9

(defun test-body-part ()
  (let* ((body-part-prototype (make-body-part-prototype 'test-head :targeting-weight 10))
         (body-part (make-body-part body-part-prototype "head")))
    (modify-damage body-part (make-damage 0 :slice 20))
    (modify-damage body-part (make-damage 0 :blunt 90))
    (describe-game-object body-part)))

(defun test-body ()
  (let ((body (make-body 'humanoid))
        (room (make-instance 'game-room)))
    (move-to-room body room)
    (modify-damage (body-part body "neck")        (make-damage 0 :slice 20))
    (modify-damage (body-part body "left eye")    (make-damage 0 :pierce 90))
    (modify-damage (body-part body "left eye")    (make-damage 0 :blunt 90))
    (modify-damage (body-part body "right thigh") (make-damage 0 :blunt 30))
    (describe-game-object body)
    (look body)))

(defun test-attack ()
  (let ((attacker (make-body 'humanoid))
        (defender (make-body 'humanoid))
        (weapon   (select-item "dagger"))
        (room     (make-instance 'game-room)))
    (move-to-room defender room)
    (attack attacker defender weapon)
    (look defender)))

(defun test-target ()
  (let ((attacker (make-body 'humanoid))
        (defender (make-body 'humanoid))
        (weapon   (select-item "dagger")))
    (body-parts-for-target-selection attacker defender weapon "head")))

(defparameter giant  (make-body 'humanoid 2.3))
(defparameter hobbit (make-body 'humanoid 0.8))
(defparameter dagger (select-item "dagger"))

(defparameter rob (make-instance 'player
                           :name "rob"
                           :body giant))
(defparameter joe (make-instance 'player
                           :name "joe"
                           :body hobbit))

(defparameter office  (make-instance 'game-room))
(defparameter kitchen (make-instance 'game-room))

(move-to-room hobbit office)
(move-to-room giant  office)
