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
                                           60 "discolored"
                                           70 "bruised"
                                           80 "a sick purple-green-yellow color from deep bruising"
                                           90 "deformed, its underlying structure pulverized")

                             :pierce (plist 10 "lightly pierced"
                                            20 "pierced"
                                            30 "punctured"
                                            80 "oozing from multiple punctures"
                                            90 "covered in brutal craters, unrecognizable")

                             :fire '()
                             :ice  '()))))

(defgeneric damage-descriptions (item)
  (:documentation "Given an item, return a list of descriptions"))

;; The damage description data structure assumes that we'll never want
;; to completely remove a base data structure
;;
;; Gist is that you create an alist by appending base descriptions to
;; custom ones. Then sort the keys and use the sorted keys to find
;; which description to use. Then use assoc to get that description
(defmethod damage-descriptions ((body-part body-part))
  (let ((descriptions (append (custom-damage-descriptions body-part) (base-damage-descriptions body-part)))
        (body-part-damage (damage-received body-part)))
    (mapcar (lambda (damage-type)
              (let ((descriptions-for-type (damage-for descriptions damage-type)))
                (car (assoc (find (damage-for body-part-damage damage-type)
                                  (sort (mapcar (lambda (desc) (car desc)) descriptions-for-type) #'>) ;; sort keys descending
                                  :test (lambda (damage-received trigger-point) (>= damage-received trigger-point))) descriptions-for-type))))
            *damage-types*)))

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
(defgeneric look (game-object)
  (:documentation "Look methods correspond to the look command and will print a description of an object"))

(defmethod look ((body-part body-part))
  (labels ((describe (damage description-list)
             ;; TODO refactor
             (cdr (find (apply damage (list body-part))
                        (apply description-list (list body-part))
                        :key #'car
                        :test (lambda (damage trigger-point) (and (not (zerop damage)) (<= damage trigger-point)))))))
    (let ((damages (remove nil (mapcar (lambda (damage-type)
                                         (describe (func damage-type "-damage-received") (func "base-" damage-type "-descriptions")))
                                       '("slice" "blunt" "pierce")))))
      (look-compile (name body-part) damages))))

;; So far this is only used by the body-part look method. Wonder if it
;; should just be part of that method?
(defun look-compile (name descriptions)
  (let ((length (length descriptions)))
    (cond ((= length 1) (mkstr "Its " name " is " (car descriptions) "."))
          ((= length 2) (mkstr "Its " name " is " (car descriptions) " and " (cadr descriptions) "."))
          ((= length 3) (mkstr "Its " name " is " (car descriptions) ". It is " (cadr descriptions) " and " (caddr descriptions) ".")))))

(defmethod look ((body body))
  (remove nil (mapcar (lambda (body-part) (look (cdr body-part))) (body-parts body))))

(defun test-body-part ()
  (let ((body-part (make-instance 'body-part :name 'head)))
    (setf (blunt-damage-received body-part) 20)
    (setf (slice-damage-received body-part) 75)
    (look body-part)))

(defun test-body ()
  (let ((body (make-instance 'humanoid-body)))
    (with-accessors ((body-parts body-parts)) body
      (setf (blunt-damage-received (cdr (assoc 'head body-parts))) 20)
      (setf (slice-damage-received (cdr (assoc 'head body-parts))) 20)
      (setf (pierce-damage-received (cdr (assoc 'neck body-parts))) 40)
      (setf (slice-damage-received (cdr (assoc 'left-upper-arm body-parts))) 90))
    (look body)))
