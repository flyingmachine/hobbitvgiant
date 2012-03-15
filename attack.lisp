;; TODO is there any way to clean up the repetition of the different
;; kinds of damage? Will need to handle this for armor and toughness
;; as well.

(defmacro defattrclass (attr-name &body class-options)
  `(defclass ,(symb attr-name '-attr) ()
     ((,attr-name
       ,@class-options))))

(defmacro defattrclasses (attr-names &body class-options)
  `(loop for attr-name in ',attr-names do
        (eval (append (list 'defattrclass attr-name) '(,@class-options)))))


(defmacro defattrclasses (attr-names &body class-options)
  `(progn
     ,@(loop for attr-name in attr-names collect
            `(defattrclass ,attr-name ,@class-options))))

(defclass weapon ()
  ((base-slice-damage
    :reader base-slice-damage
    :initform 0
    :initarg :base-slice-damage)
   (base-blunt-damage
    :reader base-blunt-damage
    :initform 0
    :initarg :base-blunt-damage)
   (base-pierce-damage
    :reader base-pierce-damage
    :initform 0
    :initarg :base-pierce-damage)))

(defclass dagger (weapon)
  ((base-slice-damage
    :initform 3)
   (base-pierce-damage
    :initform 6)))

(defclass attack ()
  ((slice-damage-dealt
    :initarg :slice-damage-dealt
    :reader slice-damage-dealt)
   (blunt-damage-dealt
    :initarg :blunt-damage-dealt
    :reader blunt-damage-dealt)
   (pierce-damage-dealth
    :initarg :pierce-damage-dealt
    :reader pierce-damage-dealt)))

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
(setq dagger (make-instance 'dagger))

(defun attack (weapon target-body)
  (receive-attack (make-attack weapon) target-body)
  (look target-body))


