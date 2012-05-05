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

(defvar rob (make-instance 'player
                           :name "rob"
                           :body giant))
(defvar joe (make-instance 'player
                           :name "joe"
                           :body hobbit))

(defvar office  (make-instance 'game-room))
(defvar kitchen (make-instance 'game-room))

(pushnew rob (players office))
(pushnew joe (players office))

(attack hobbit giant dagger)
