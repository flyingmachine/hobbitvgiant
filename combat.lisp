

(defmethod generate-damage (weapon)
  (labels ((damage (max)
             (if (zerop max)
                 0
                 (let ((min (ceiling (/ max 2))))
                   (+ min (random (1+ (- max min))))))))
    (mapcar (lambda (dam-max) (damage dam-max))
            (list (base-slice-damage weapon) (base-blunt-damage weapon) (base-pierce-damage weapon)))))

;; TODO add body so that you can calc modifiers like str, agi
;; TODO handle damage in a better way.. car/cadr etc are not straightforward
(defmethod make-attack (weapon)
  (let ((damage (generate-damage weapon)))
    (make-instance 'attack
                   :slice-damage-dealt (car damage)
                   :blunt-damage-dealt (cadr damage)
                   :pierce-damage-dealt (caddr damage))))

(defmethod receive-attack (attack body)
  (with-accessors ((body-parts body-parts)) body
    (incf (slice-damage-received (cdr (assoc 'head body-parts))) (slice-damage-dealt attack))
    (incf (blunt-damage-received (cdr (assoc 'head body-parts))) (blunt-damage-dealt attack))
    (incf (pierce-damage-received (cdr (assoc 'head body-parts))) (pierce-damage-dealt attack))))

(setq giant (make-instance 'humanoid-body))


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
            (incf-damage (damage-received body-part) damage-type (damage-for weapon-damage damage-type)))
          *damage-types*)))
