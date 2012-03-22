(defun attack (attacker defender weapon &optional target)
  (let ((thing-hit (attempt-hit attacker defender target weapon)))
    (if thing-hit
        ;; TODO have more sophisticated damage calc
        (apply-damage attacker defender weapon thing-hit)
        (format t "You missed! How sad.~%"))))

(defun attempt-hit (attacker defender weapon &optional target)
  ;; using merge-alists seems kinda silly here
  (let* ((body-part-weights (symmetrize-body-parts *asym-humanoid-body-parts*))
         (miss (= (random 10) 0)))
    (unless miss
      (car (select-target body-part-weights)))))

(defun apply-damage (attacker defender weapon thing-hit)
  (let ((body-part (assocdr thing-hit (body-parts defender)))
        (weapon-damage (active-damage-set weapon)))
    (mapc (lambda (damage-type)
            (incf (damage-for (damage-received body-part) damage-type) (random-damage (damage-for weapon-damage damage-type))))
          *damage-types*)))

;; will probably end up making this more general
(defun random-damage (base-damage)
  (case base-damage
    (0 0)
    (1 1)
    (2 (1+ (random 2)))
    (otherwise (+ (1+ (floor (/ base-damage 2))) (random (ceiling (/ base-damage 2))))))))
