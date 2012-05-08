(in-package :hobbitvgiant)

;; Target is the string name of a specific body part
;; returns # of AP points expended
(defun attack (attacker defender weapon &optional target)
  (let ((thing-hit (attempt-hit attacker defender weapon target)))
    (if thing-hit
        (apply-damage attacker defender weapon thing-hit)
        (format t "You missed! How sad.~%"))
    3))

(defun attempt-hit (attacker defender weapon &optional target)
  (let ((miss (= (random (chance-to-miss attacker defender weapon target)) 0)))
    (unless miss
      (cdr (select-target (body-parts-for-target-selection attacker defender weapon target) #'body-part-targeting-weight-sum)))))

;; If attacker is twice as tall as defender, increase chance to miss
(defun chance-to-miss (attacker defender weapon &optional target)
  (if (> (/ (height attacker) (height defender)) 2)
      10 ;; 1 in 10 chance of missing
      15 ;; 1 in 15 chance of missing
      ))

(defun apply-damage (attacker defender weapon body-part)
  (modify-damage body-part (damage-dealt (active-damage-set weapon))))

;; TODO is there some way to use closures instead of the damage types global?
(defun damage-dealt (weapon-damage)
  (let ((damage-arg-list (mappend (lambda (damage-type)
                                    (list damage-type (random-damage (damage-for weapon-danage damage-type))))
                                  *damage-types*)))
    (apply #'make-damage (append (list 0) damage-arg-list))))

;; will probably end up making this more general
(defun random-damage (base-damage)
  (case base-damage
    (0 0)
    (1 1)
    (2 (1+ (random 2)))
    (otherwise (+ (1+ (floor (/ base-damage 2))) (random (ceiling (/ base-damage 2)))))))

;;---
;; Targeting
;;---

;; adjusts for height and for body part selected
;; if out of reach, half as likelye to hit
;; if targeting, 12x as likely to hit
;; if out of reach and targeting, 8x as likely to hit
;;
;; TODO make this skill / attribute based
(defun body-parts-for-target-selection (attacker defender weapon &optional target)
  (let1 reach (+ (item-length weapon) (height attacker))
    (labels ((build (layers acc acc-height)
               (if (null layers)
                   acc
                   (build (cdr layers)
                          (append acc (mappend (lambda (body-part)
                                                 (list (cons (cond
                                                               ((equal (name body-part) target) (* (if (< reach acc-height) 8 12) (targeting-weight body-part)))
                                                               ((< reach acc-height) (/ (targeting-weight body-part) 2))
                                                               (t  (targeting-weight body-part)))
                                                             body-part)))
                                               (body-parts (cdar layers))))
                          (+ acc-height (height (cdar layers)))))))
      (build (body-layers defender) nil 0))))

(defun body-part-targeting-weight-sum (body-parts-and-weights)
  (coerce (reduce #'+ body-parts-and-weights :key #'car) 'float))

(defun select-target (weights-and-items sum-function)
  (nth (position
        (random (funcall sum-function weights-and-items))
        weights-and-items
        :key #'car
        :test (target-hit-function))
       weights-and-items))

;; wonder if it's good style to include "function" when returning function
(defun target-hit-function ()
  (let1 current-position 0
    (lambda (target increment)
      (incf current-position increment)
      (> current-position target))))

;;---
;; Combat actions
;;---

;; Must return number of AP expended, including negative AP for
;; spells, item usage that increase AP
;; current is a body
(defun combat-action (current engagees)
  (if (player current)
      (player-combat-action current engagees)
      (mob-combat-action current engagees)))
 
(defun mob-combat-action (current engagees)
  (let1 first-target (find current engagees :test (lambda (a b) (not (eql a b))))
    (attack current first-target (current-weapon current))))
