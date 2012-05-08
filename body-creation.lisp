(in-package :hobbitvgiant)

(defun make-body (template-name &optional (scale 1))
  (let ((template (gethash template-name *body-templates*))
        (body (make-instance 'body
                             :scale scale)))

    ;; create layers and associate them with body
    (let1 layers (create-body-layers
                  body
                  template
                  (create-parts-from-prototype-pairs (mappend #'third template) *body-part-prototypes*))
      (setf (body-layers body) layers)
      (mapc (lambda (layer)
              (setf (body layer) body))
            (mapcar #'cdr layers)))


    (macrolet ((observe-attributes (&rest attributes)
                 `(progn
                    ,@(mapcar (lambda (attribute)
                                `(observe (body ,attribute 'room-notifier new old)
                                   (setf (latest-event (game-room body)) (list (id body) (list ,attribute new))))) attributes))))
      (observe-attributes 'strength 'stamina 'agility 'dexterity))
    
    body))

;; Associate bodyparts with layers and set base to previously created layer
(defun create-body-layers (body template body-parts)
  (labels ((compose (layers acc)
             (if (consp layers)
                 (let* ((layer           (car layers))
                        (layer-name      (first layer))
                        (height          (* (scale body) (second layer)))
                        (body-part-names (mapcar #'cdr (third layer))))
                   (compose
                    (cdr layers)
                    (append1 acc (cons layer-name (make-body-layer body (parts-for-layer body-part-names body-parts) height (cdr (last acc)))))))
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
