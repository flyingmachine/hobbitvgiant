(defun attack (attacker defender weapon &optional target)
  (let ((thing-hit (attempt-hit attacker defender weapon target)))
    (if thing-hit
        (apply-damage attacker defender weapon thing-hit)
        (format t "You missed! How sad.~%"))))

;; Target is a specific body part
(defun attempt-hit (attacker defender weapon &optional target)
  (let ((miss (= (random 10) 0)))
    (unless miss
      (cdr (select-target (body-parts-for-target-selection attacker defender weapon target))))))

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

;; adjusts for height and for body part selected
;; if out of reach, half as likelye to hit
;; if targeting, 12x as likely to hit
;; if out of reach and targeting, 8x as likely to hit
;;
;; TODO make this skill / attribute based
(defun body-parts-for-target-selection (attacker defender weapon &optional target)
  (let ((reach (+ (item-length weapon) (height attacker))))
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
  (reduce #'+ body-parts-and-weights :key #'car))

(defun select-target (body-parts-and-weights)
  (nth (position
        (random (body-part-targeting-weight-sum body-parts-and-weights))
        body-parts-and-weights
        :key #'car
        :test (target-hit-function))
       body-parts-and-weights))

;; wonder if it's good style to include "function" when returning function
(defun target-hit-function ()
  (let ((current-position 0))
    (lambda (target increment)
      (incf current-position increment)
      (> current-position target))))
