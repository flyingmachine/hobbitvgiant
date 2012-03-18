(defclass attack ()
  (damage)
  (target))

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


(defun attack (attacker recipient target weapon)
  (let thing-hit))
