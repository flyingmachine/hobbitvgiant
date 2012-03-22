;; ---
;; Damage
;; ---
(defparameter *damage-types* '(slice blunt pierce fire ice))
(defparameter *damage-set-types* '(1h 2h thrown))

;; create a damage hash
(defun make-damage (default &key slice blunt pierce fire ice)
  (macrolet ((setter ()
               `(progn
                  ,@(mapcar (lambda (dtype)
                              `(setf (gethash ',dtype d) (or ,dtype default))) *damage-types*))))
    (let ((d (make-hash-table)))
      (setter)
      d)))

(defun damage-for (damage damage-type)
  (gethash damage-type damage))

(defun set-damage-for (damage damage-type val)
  (setf (gethash damage-type damage) val))

(defsetf damage-for set-damage-for)

;; ---
;; Status Effects
;; ---
(defparameter *status-effects* '(poison stun blind bound paralyzed))
