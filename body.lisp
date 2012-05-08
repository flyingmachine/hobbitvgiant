(in-package :hobbitvgiant)

;; Bodies
;; ======
;; 
;; Each body is composed of body parts
;;
;; Each body part has a prototype
;; 
;; The prototype is a global object with invariant data, like
;; targeting weight and damage descriptions. For example, all eyeballs
;; should have the same descriptions when they've sustained the same
;; degree of damage.
;;
;; The body part object contains instance-specific data, like how much
;; damage is actually sustained
;;
;; Each time a body is created, new body parts are composed with their
;; prototypes using body templates. A human would have a different
;; template from a spider.

;; ---
;; bodies
;; ---
(defclass body (game-object named-object)
  ((body-layers
    :initarg :body-layers
    :accessor body-layers)
   
   (scale
    :documentation "How large or small the body is relative to a 'standard' body. Affects overall height and layer height."
    :initarg :scale
    :initform 1
    :reader scale)

   (strength
    :documentation "Used to determine:
* Carrying capacity
* Damage dealt with strength-favoring weapons"
    :initarg :strength
    :initform 1
    :accessor strength)

   (stamina
    :documentation "Used to determine:
* Fatigue capacity / recovery
* Health
* Resistance to effects"
    :initarg :stamina
    :initform 1
    :accessor stamina)

   (agility
    :documentation "Used to determine:
* Swiftness of blows - AP
* Dodge
* Damage dealt with agility-favoring weapons"
    :initarg :agility
    :initform 1
    :accessor agility)

   (dexterity
    :documentation "Used to determine:
* Spellcasting
* Binding wounds
* Lockpicking
* Stealing
* Manual tasks..."
    :initarg :dexterity
    :initform 1
    :accessor dexterity)

   (player
    :documentation "Used to determine whether to prompt for action during combat"
    :initarg :player
    :initform nil
    :accessor player)

   (game-room
    :documentation "The current location of the body"
    :initarg :game-room
    :initform nil
    :accessor game-room))
  (:metaclass observable))

(defun body-part (body part-name)
  (find part-name (body-parts body) :key #'name :test #'equal))

(defun body-part-targeting-weights (body)
  (alist-by-func (body-parts body) #'targeting-weight))

(defmethod body-parts ((body body))
  (mappend (lambda (layer) (body-parts (cdr layer))) (body-layers body)))

(defmethod height ((body body))
  (reduce #'+ (mapcar #'cdr (body-layers body)) :key #'height))

(defmethod max-health ((body body))
  (* 20 (stamina body)))

(defmethod max-ap ((body body))
  (agility body))

(defmethod dead? ((body body))
  (<= (current-health body) 0))

(defun current-weapon (body)
  (select-item "dagger"))

(defmethod current-health ((body body))
  (- (max-health body) (reduce #'+ (mappend (lambda (bp) (hash-values (damage-received bp))) (body-parts body)))))

(defmethod player? ((body body))
  (not (null (player body))))

;; TODO should this be a before method that doesn't do
;; call-next-method if the body belongs to a player?
(defmethod name ((body body))
  (if (player? body)
      (name (player body))
      (call-next-method)))

(defmethod serialize ((body body))
  (list (list 'name (name body))
        (list 'current-health (current-health body))
        (list 'body-parts (mapcar #'serialize (body-parts body)))))
