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
;; would like damage-sets to be a alist
;;
;; Each weapon has weapon stats, which can contain multiple damage
;; sets. For example, a throwing knife could have melee and thrown
;; damage sets.
(defclass weapon-stats ()
  ((damage-sets
    :initarg :damage-sets
    :reader  damage-sets)
   (active-damage-set
    :accessor active-damage-set
    :initform 'melee)))

(defmethod active-damage-set ((weapon item))
  (with-accessors ((weapon-stats weapon-stats)) weapon
    (gethash (active-damage-set weapon-stats) (damage-sets weapon-stats))))

(defmethod initialize-instance :after ((weapon-stats weapon-stats) &key)
  (let ((dsets (make-hash-table)))
    (mapc (lambda (pair)
            (print (car pair))
            (print (cdr pair))
            (setf (gethash (car pair) dsets) (apply #'make-damage 0 (cdr pair))))
          (pairs (slot-value weapon-stats 'damage-sets)))
    (setf (slot-value weapon-stats 'damage-sets) dsets)))


;; TODO be able to specify multiple attack types, like 1h, 2h, thrown, etc
(defun make-weapon (name description &rest damage-options)
  (make-instance 'item
                 :name name
                 :description description
                 :weapon-stats (make-instance 'weapon-stats
                                              :damage-sets damage-options)))

(defparameter *weapons* `((dagger . ,(make-weapon "dagger"
                                                  "A dagger! It's pointy!"
                                                  'melee
                                                  '(:slice 2
                                                    :pierce 6)))
                          (mace   . ,(make-weapon "mace"
                                                  "A mace! It's blunty!"
                                                  'melee
                                                  '(:blunt 10
                                                    :pierce 2)))))

(defun select-weapon (weapon)
  (assocdr weapon *weapons*))
