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
  (let1 body-part (make-instance 'body-part
                                 :prototype prototype
                                 :name name)
    (observe (body-part 'damage-received 'room-notifier)
      (let1 body (body body-part)
        (setf (latest-event (game-room body))
              (list (id body)
                    (list 'body-parts
                          (list (serialize body-part)))
                    (list 'current-health
                          (current-health body))))))
    
    body-part))

(defgeneric modify-damage (object damage)
  (:documentation "Adds 'modification' to the damage type of a damage object associated with a game object"))

(defmethod modify-damage ((body-part body-part) damage)
  (mergehash (damage-received body-part) damage #'+))

(defmethod modify-damage :around ((body-part body-part) damage)
  (let ((old (damage-received body-part))
        (new (call-next-method)))
    (call-observers (observers body-part 'damage-received) new old)
    new))

(defmacro defproxy (proxy-name proxied-name method-name)
  `(defmethod ,method-name ((,proxy-name ,proxy-name))
     (,method-name (,proxied-name ,proxy-name))))

(defproxy body-part body-layer body)
(defproxy body-part prototype damage-descriptions)
(defproxy body-part prototype targeting-weight)

(defgeneric serialize (object)
  (:documentation "Represent object and its attributes as a list"))

;; TODO how can I eliminate the need for this?
(defmethod serialize ((body-part body-part))
  (list (list 'name (name body-part))
        (list 'damage-received
              (mapcar (lambda (damage-type)
                        (list damage-type (damage-for (damage-received body-part) damage-type)))
                      *damage-types*))))
