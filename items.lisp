(defclass item ()
  ((weapon-stats
    :initarg :weapon-stats
    :reader weapon-stats)
   (name
    :initarg :name
    :reader name)
   (description
    :initarg :description
    :accessor description)))

;; eventually add support for enchantments
;; would like damage-sets to be a plist
(defclass weapon-stats ()
  ((damage-sets
    :initarg :damage-sets
    :reader  damage-sets)))

(defmethod initialize-instance :after ((weapon-stats weapon-stats) &key)
  (setf (slot-value weapon-stats 'damage-sets)
        (apply #'vector (mapcar (lambda (damage-set)
                                  (apply #'make-instance (append '(damage) damage-set)))
                                (slot-value weapon-stats 'damage-sets)))))

;; TODO be able to specify multiple attack types, like 1h, 2h, thrown, etc
(defun make-weapon (name description &rest damage-options)
  (make-instance 'item
                 :name name
                 :description description
                 :weapon-stats (make-instance 'weapon-stats
                                              :damage-sets damage-options)))

(defparameter *weapons* `((dagger . ,(make-weapon "dagger"
                                                  "A dagger! It's pointy!"
                                                  '(:slice 2
                                                    :pierce 6)))
                          (mace   . ,(make-weapon "mace"
                                                  "A mace! It's blunty!"
                                                  '(:blunt 10
                                                    :pierce 2)))))
