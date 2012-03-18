(defclass body-part ()
  ((damage-received
    :initarg :damage-received
    :initform (make-instance 'damage)
    :accessor damage-received)
   
   (name
    :initarg :name
    :reader name)

   (custom-damage-descriptions
    :allocation :class
    :reader custom-damage-descriptions
    :initform '())
   
   (base-damage-descriptions
    :allocation :class
    :reader base-damage-descriptions
    :initform (make-instance 'damage
                             :slice (plist 10 "lightly scratched"
                                           20 "scratched"
                                           50 "cut"
                                           60 "deeply cut"
                                           70 "marred by multiple cuts"
                                           80 "covered in deep, glistening gashes"
                                           90 "a ragged mess of flesh with deep lacerations crisscrossing it, exposing bone" )

                             :blunt (plist 10 "slightly discolored"
                                           20 "discolored"
                                           40 "bruised"
                                           70 "a sick purple-green-yellow color from deep bruising"
                                           90 "deformed, its underlying structure pulverized")

                             :pierce (plist 10 "lightly pierced"
                                            20 "pierced"
                                            30 "punctured"
                                            80 "oozing from multiple punctures"
                                            90 "covered in brutal craters, unrecognizable")

                             :fire '()
                             :ice  '()))))



;; The damage description data structure assumes that we'll never want
;; to completely remove a base data structure
;;
;; Gist is that you create an alist by appending base descriptions to
;; custom ones. Then sort the keys and use the sorted keys to find
;; which description to use. Then use assoc to get that description
;;
;; If I do end up having item damage descriptions, will probably use
;; the same code
(defmethod describe-damage ((body-part body-part))
  (let ((descriptions (append (custom-damage-descriptions body-part) (base-damage-descriptions body-part)))
        (body-part-damage (damage-received body-part)))
    (remove nil (mapcar (lambda (damage-type)
                          (let ((descriptions-for-type (damage-for descriptions damage-type)))
                            (second (assoc (find (damage-for body-part-damage damage-type)
                                                 (sort (mapcar (lambda (desc) (car desc)) descriptions-for-type) #'>) ;; sort keys descending
                                                 :test (lambda (damage-received trigger-point) (>= damage-received trigger-point))) descriptions-for-type))))
                        *damage-types*))))

(defclass body ()
  ((body-parts
    :documentation "An alist of body parts, (name . body-part-instance)")))

(defclass humanoid-body (body)
  ((body-parts
    :initform (mapcar (lambda (body-part)
                        (cons (car body-part) (make-instance 'body-part :name (car body-part))))
                      (symmetrize-body-parts *asym-humanoid-body-parts*))
    :reader body-parts)))

;; Does it make sense tao have a generic describe method for like
;; everything in the game?
(defgeneric describe-game-object (game-object)
  (:documentation "Compiles the final description for an object"))

(defmethod describe-game-object ((body-part body-part))
  (let ((descriptions (describe-damage body-part)))
    (when descriptions
      (let ((preamble (mkstr "Its " (name body-part) " is")))
        (if (second descriptions)
            (append (list preamble) descriptions)
            (list (mkstr preamble " " (first descriptions))))))))


(defmethod describe-game-object ((body body))
  (remove nil (mapcar (lambda (body-part) (describe-game-object (cdr body-part))) (body-parts body))))


(defun body-part (body part-name)
  (cdr (assoc part-name (body-parts body))))

(defun test-body-part ()
  (let ((body-part (make-instance 'body-part :name 'head)))
    (set-damage (damage-received body-part) slice 20)
    (set-damage (damage-received body-part) blunt 75)
    (describe-game-object body-part)))

(defun test-body ()
  (let ((body (make-instance 'humanoid-body)))
    (set-damage (damage-received (body-part body 'neck)) slice 20)
    (set-damage (damage-received (body-part body 'left-eye)) pierce 90)
    (set-damage (damage-received (body-part body 'left-eye)) blunt 90)
    (set-damage (damage-received (body-part body 'right-thigh)) blunt 30)
    (describe-game-object body)))
