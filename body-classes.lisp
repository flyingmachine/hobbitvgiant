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

(defparameter *default-damage-descriptions*
  (make-damage nil
               :slice (pairsr 1  "lightly scratched"
                              10 "scratched"
                              30 "cut"
                              60 "deeply cut"
                              70 "marred by multiple cuts"
                              80 "covered in deep, glistening gashes"
                              90 "a ragged mess of flesh with deep lacerations crisscrossing it, exposing bone" )
               
               :blunt (pairsr 1  "slightly discolored"
                              10 "discolored"
                              40 "bruised"
                              70 "a sick purple-green-yellow color from deep bruising"
                              90 "deformed, its underlying structure pulverized")
               
               :pierce (pairsr 1  "pricked"
                               10 "pierced"
                               30 "punctured"
                               80 "oozing from multiple punctures"
                               90 "covered in brutal craters, unrecognizable")
               
               :fire (pairsr 1  "singed"
                             10 "a light pink-red from heat"
                             20 "burnt"
                             40 "red and burnt"
                             50 "badly burnt"
                             60 "very badly burnt"
                             70 "bubbling"
                             80 "covered in boils and turning black in spots"
                             90 "completely charred, with bits of flesh flaking off")
               :ice  '()))

;;---
;; body part prototypes
;;---

;; This isn't really prototypal in the way that javascript is
;; prototypal because ultimately we don't "shadow" any variables
(defclass body-part-prototype ()
  ((targeting-weight
    :documentation "How likely it is to hit this body part relative to other body parts"
    :initarg :targeting-weight
    :reader targeting-weight)
   
   (damage-descriptions
    :documentation "How the body part should 'look' after receiving various types of damage"
    :initarg :damage-descriptions
    :reader damage-descriptions
    :initform *default-damage-descriptions*)))

(defun make-body-part-prototype (name &key targeting-weight (damage-descriptions *default-damage-descriptions*))
  (setf (gethash name *body-part-prototypes*)
        (make-instance 'body-part-prototype
                       :targeting-weight targeting-weight
                       :damage-descriptions damage-descriptions)))

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
    :accessor damage-received))
  (:metaclass observable))

(defun make-body-part (prototype name)
  (make-instance 'body-part
                 :prototype prototype
                 :name name))

;; TODO modify so it takes pairs of dtype, value
(defgeneric modify-damage (game-object damage-type modification)
  (:documentation "Adds 'modification' to the damage type of a damage object associated with a game object"))

(defmethod modify-damage ((body-part body-part) damage-type modification)
  (incf (damage-for (damage-received body-part) damage-type) modification))

(defmethod modify-damage :around ((body-part body-part) damage-type modification)
  (let ((old (damage-for (damage-received body-part) damage-type))
        (new (call-next-method)))
    (when (not (eql new old))
      (call-observers (observers body-part 'damage-received) new old))
    new))

(defmacro defproxy (proxy-name proxied-name method-name)
  `(defmethod ,method-name ((,proxy-name ,proxy-name))
     (,method-name (,proxied-name ,proxy-name))))

(defproxy body-part prototype damage-descriptions)
(defproxy body-part prototype targeting-weight)

;; ---
;; body layers
;; ---
(defclass body-layer ()
  ((body
    :documentation "The body this belongs to. Used for scale."
    :initarg :body
    :reader body)
   
   (body-parts
    :documentation "The body parts for this layer"
    :initarg :body-parts
    :reader body-parts)

   (height
    :initarg :height
    :reader  height)

   (base
    :documentation "Layer it sits on top of"
    :initarg :base
    :reader  base)))

(defun make-body-layer (body-parts height base)
  (make-instance 'body-layer
                 :body-parts body-parts
                 :height height
                 :base base))

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

(defun make-body (template-name &optional (scale 1))
  (let ((template (gethash template-name *body-templates*))
        (body (make-instance 'body
                             :scale scale)))
    (setf (body-layers body)
          (create-body-layers
           (scale body)
           template
           (create-parts-from-prototype-pairs (mappend #'third template) *body-part-prototypes*)))

    (macrolet ((observe-attributes (&rest attributes)
                 `(progn
                    ,@(mapcar (lambda (attribute)
                                `(observe (body ,attribute 'room-notifier new old)
                                   (setf (latest-event (game-room body)) (list body (list ,attribute new))))) attributes))))
      (observe-attributes 'strength 'stamina 'agility 'dexterity))
    
    (observe-each ((body-parts body) body-part 'damage-received 'room new)
      (setf (latest-event (game-room body))
            (list body
                  (list 'body-parts
                        (list body-part new))
                  (list 'current-health
                        (current-health body)))))
    
    body))

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

;; Associate bodyparts with layers and set base to previously created layer
(defun create-body-layers (scale template body-parts)
  (labels ((compose (layers acc)
             (if (consp layers)
                 (let* ((layer           (car layers))
                        (layer-name      (first layer))
                        (height          (* scale (second layer)))
                        (body-part-names (mapcar #'cdr (third layer))))
                   (compose
                    (cdr layers)
                    (append1 acc (cons layer-name (make-body-layer (parts-for-layer body-part-names body-parts) height (cdr (last acc)))))))
                 acc)))
    (compose template nil)))

(defun create-parts-from-prototype-pairs (prototype-pairs prototype-set)
  (mapcar (lambda (prototype-pair)
            (make-body-part (gethash (car prototype-pair) prototype-set) (cdr prototype-pair)))
          prototype-pairs))

;; TODO refactor out this test function?
;; since layers only store body part names, we need a way of
;; re-associating the actual layer object with the created body part object
(defun parts-for-layer (body-part-names body-parts)
  (remove-if-not (lambda (name) (position name body-part-names)) body-parts :key #'name))

(defun body-part (body part-name)
  (find part-name (body-parts body) :key #'name :test #'equal))

(defun body-part-targeting-weights (body)
  (alist-by-func (body-parts body) #'targeting-weight))

(defun alist-by-func (collection func)
  (mapcar (lambda (member)
            (cons member (funcall func member)))
          collection))
