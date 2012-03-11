(defclass body-part ()
  ((base-slice-descriptions
    :allocation :class
    :reader base-slice-descriptions
    :initform '((0  . nil)
                (10 . "lightly scratched")
                (20 . "scratched")
                (50 . "cut")
                (60 . "deeply cut")
                (70 . "marred by multiple cuts")
                (80 . "covered in deep, glistening gashes")
                (90 . "a ragged mess of flesh with deep lacerations crisscrossing it, exposing bone")))
 
   (base-blunt-descriptions
    :allocation :class
    :reader base-blunt-descriptions
    :initform '((0  . nil)
                (10 . "slightly discolored")
                (60 . "discolored")
                (70 . "bruised")
                (80 . "a sick purple-green-yellow color from deep bruising")
                (90 . "deformed, its underlying structure pulverized")))
   
   (base-pierce-descriptions
    :allocation :class
    :reader base-pierce-descriptions
    :initform '((0  . nil)
                (10 . "lightly pierced")
                (60 . "scratched")
                (70 . "cut")
                (80 . "oozing from multiple punctures")
                (90 . "brutally lacerated, unrecognizable")))

   (slice-damage
    :initarg :slice-damage
    :initform 0
    :accessor slice-damage)
   (blunt-damage
    :initarg :blunt-damage
    :initform 0
    :accessor blunt-damage)
   (pierce-damage
    :initarg :pierce-damage
    :initform 0
    :accessor pierce-damage)))

(defclass body ()
  ((body-parts)))

(defclass humanoid-body (body)
  ((body-parts
    :initform (mapcar (lambda (body-part)
                        (cons (car body-part) (make-instance 'body-part)))
                      (symmetrize-body-parts *asym-humanoid-body-parts*))
    :reader body-parts)))

;; Does it make sense to have a generic describe method for like
;; everything in the game?
(defgeneric look (game-object))

(defmethod look ((body-part body-part))
  (labels ((describe (damage description-list)
             ;; TODO refactor
             (find (apply damage (list body-part)) (apply description-list (list body-part)) :key #'car :test (lambda (damage trigger-point) (<= damage trigger-point)))))
    (nconc (mapcar (lambda (damage-type)
                     (describe (func damage-type "-damage") (func "base-" damage-type "-descriptions")))
                   '("slice" "blunt" "pierce")))))

(defmethod look ((body body))
  )

(defun test-body-part ()
  (let ((body-part (make-instance 'body-part)))
    (setf (blunt-damage body-part) 20)
    (setf (slice-damage body-part) 75)
    (look body-part)))
