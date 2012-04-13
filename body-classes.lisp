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

;; This isn't really prototypal in the way that javascript is
;; prototypal because ultimately we don't "shadow" any variables

;; TODO separate name from identifier?
(defclass body-part-prototype ()
  ((name
    :documentation "Name of body part, e.g. head, foot, etc"
    :initarg :name
    :reader name)

   (targeting-weight
    :documentation "How likely it is to hit this body part relative to other body parts"
    :initarg :targeting-weight
    :reader targeting-weight)
   
   (damage-descriptions
    :documentation "How the body part should 'look' after receiving various types of damage"
    :initarg :damage-descriptions
    :reader damage-descriptions
    :initform *default-damage-descriptions*)))

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

(defclass body ()
  ((body-parts
    :initarg :body-parts
    :reader body-parts)
   
   (scale
    :documentation "How large or small the body is relative to a 'standard' body"
    :initarg :scale
    :initform 1
    :reader scale)))
