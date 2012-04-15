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

;; TODO add in a body graph to handle height
;; each node is a body part, each edge value is length?
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
(defclass body-part ()
  ((prototype
    :initarg :prototype
    :accessor prototype)

   (name
    :initarg :name
    :reader  name)

   (damage-received
    :initarg :damage-received
    :initform (make-damage 0)
    :accessor damage-received)))

(defun make-body-part (prototype name)
  (make-instance 'body-part
                 :prototype prototype
                 :name name))

(defgeneric modify-damage (game-object damage-type modification)
  (:documentation "Adds 'modification' to the damage type of a damage object associated with a game object"))

(defmethod modify-damage ((body-part body-part) damage-type modification)
  (incf (damage-for (damage-received body-part) damage-type) modification))

(defmacro defproxy (proxy-name proxied-name method-name)
  `(defmethod ,method-name ((,proxy-name ,proxy-name))
     (,method-name (,proxied-name ,proxy-name))))

(defproxy body-part prototype damage-descriptions)
(defproxy body-part prototype targeting-weight)

;; ---
;; bodies
;; ---
(defclass body ()
  ((body-parts
    :initarg :body-parts
    :reader body-parts)
   
   (scale
    :documentation "How large or small the body is relative to a 'standard' body"
    :initarg :scale
    :initform 1
    :reader scale)))

(defun make-body (template-name &optional (scale 1))
  (make-instance 'body
                 :body-parts (compose-parts-from-template template-name)
                 :scale scale))

(defun compose-parts-from-template (template-name)
  (mapcar (lambda (prototype-pair)
            (let ((prototype (gethash (car prototype-pair) *body-part-prototypes*)))
              (if (cdr prototype-pair)
                  (make-body-part prototype (cdr prototype-pair))
                  (make-body-part prototype (string-downcase (mkstr (car prototype-pair)))))))
          (gethash template-name *body-templates*)))

(defun body-part (body part-name)
  (find part-name (body-parts body) :key #'name :test #'equal))

(defun body-part-targeting-weights (body)
  (mapcar (lambda (body-part)
            (cons body-part (targeting-weight (prototype (body-part)))))
          (body-parts body)))
