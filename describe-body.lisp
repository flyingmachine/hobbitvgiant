(defclass body-part ()
  ((base-slice-descriptions
    :allocation :class
    :reader base-slice-descriptions
    :initform '((90 . (lightly scratched))
                (80 . (scratched))
                (70 . (cut))
                (60 . (deeply cut))
                (50 . (marred by multiple cuts))
                (20 . (covered in deep glistening gashes))
                (10 . (a ragged mess of flesh with deep lacerations crisscrossing it leaving bone exposed))))
   (base-blunt-descriptions
    :allocation :class
    :reader base-blunt-descriptions
    :initform '((90 . (slightly discolored))
                (80 . (discolored))
                (70 . (bruised))
                (60 . (a sick purple-green-yellow color from deep bruising))
                (10 . (deformed its underlying structure pulverized))))
   (base-pierce-descriptions
    :allocation :class
    :reader base-pierce-descriptions
    :initform '((90 . (lightly pierced))
                (80 . (scratched))
                (70 . (cut))
                (60 . (oozing from multiple punctures))
                (10 . (brutally lacerated unrecognizable))))

   (slice-health
    :initarg :slice-health
    :initform 100
    :reader slice-health)
   (blunt-health
    :initarg :blunt-health
    :initform 100
    :reader blunt-health)
   (pierce-health
    :initarg :pierce-health
    :initform 100
    :reader pierce-health)))

(defclass body ()
  ((body-parts)))

(defclass humanoid-body (body)
  ((body-parts
    :initform (mapcar (lambda (body-part)
                        (cons (car body-part) (make-instance body-part))) (symmetrize-body-parts *asym-body-parts*)))))

(defun string-function (base &key prefix suffix)
  (symbol-function (intern (string-upcase (concatenate 'string prefix base suffix)))))

(defgeneric describe-body-part (body-part))

(defmethod describe-body-part (body-part)
  (labels ((describe (health description-list)
             (find (apply health (list body-part)) (apply description-list (list body-part)) :key #'car :test (lambda (health trigger-point) (> trigger-point health)))))
    (append (mapcar (lambda (damage-type)
                      (describe (string-function damage-type :suffix "-health") (string-function damage-type :prefix "base-" :suffix "-descriptions")))
                    '("slice" "blunt" "pierce")))))

(defgeneric describe-body (body)
  )

