(defun attack (attacker defender weapon &optional target)
  (let ((thing-hit (attempt-hit attacker defender target weapon)))
    (if thing-hit
        (apply-damage attacker defender weapon thing-hit)
        (format t "You missed! How sad.~%"))))

;; Target is a specific body part
(defun attempt-hit (attacker defender weapon &optional target)
  (let ((miss (= (random 10) 0)))
    (unless miss
      (select-target (body-parts defender)))))

(defun apply-damage (attacker defender weapon body-part)
  (let ((weapon-damage (active-damage-set weapon)))
    (mapc (lambda (damage-type)
            (modify-damage body-part
                           damage-type
                           (random-damage (damage-for weapon-damage damage-type))))
          *damage-types*)))

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
(defun body-part-sum (body-parts)
  (reduce #'+ body-parts :key #'targeting-weight))

(defun select-target (body-parts)
  (nth (position (random (body-part-sum body-parts)) body-parts :key #'targeting-weight :test (target-hit-function)) body-parts))

;; wonder if it's good style to include "function" when returning function
(defun target-hit-function ()
  (let ((current-position 0))
    (lambda (target increment)
      (incf current-position increment)
      (> current-position target))))
