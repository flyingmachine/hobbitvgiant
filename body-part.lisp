(in-package :hobbitvgiant)

;; ---
;; body parts
;; ---
(defclass body-part (game-object named-object)
  ((prototype
    :initarg :prototype
    :accessor prototype)

   (damage-received
    :initarg :damage-received
    :initform (make-damage 0)
    :accessor damage-received)
   (body-layer
    :documentation "Body layer this belongs to"
    :initarg :body-layer
    :accessor body-layer))
  (:metaclass observable))

(defun make-body-part (prototype name)
  (make-instance 'body-part
                 :prototype prototype
                 :name name))

;; TODO modify so it takes pairs of dtype, value
(defgeneric modify-damage (game-object damage)
  (:documentation "Adds 'modification' to the damage type of a damage object associated with a game object"))

(defmethod modify-damage ((body-part body-part) damage)
  (incf (damage-for (damage-received body-part) damage-type) modification))

(defmethod modify-damage :around ((body-part body-part) damage)
  (let ((old (damage-for (damage-received body-part) damage-type))
        (new (call-next-method)))
    (when (not (eql new old))
      (call-observers (observers body-part 'damage-received) new old))
    new))

(defmacro defproxy (proxy-name proxied-name method-name)
  `(defmethod ,method-name ((,proxy-name ,proxy-name))
     (,method-name (,proxied-name ,proxy-name))))

(defproxy body-part body-layer body)
(defproxy body-part prototype damage-descriptions)
(defproxy body-part prototype targeting-weight)


;; TODO how can I eliminate the need for this?
(defmethod serialize ((body-part body-part))
  (list (list 'name (name body-part))
        (list 'damage-received
              (mapcar (lambda (damage-type)
                        (list damage-type (damage-for (damage-received body-part) damage-type)))
                      *damage-types*))))
