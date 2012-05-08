(in-package :hobbitvgiant)

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
