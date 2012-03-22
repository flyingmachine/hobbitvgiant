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
;;
;; TODO does balance change when switching 1h/2h?
(defclass weapon-stats ()
  ((damage-sets
    :initarg :damage-sets
    :reader  damage-sets)
   (active-damage-set
    :accessor active-damage-set
    :initform '1h)))

(defmethod active-damage-set ((weapon item))
  (:documentation "Weapons can have multiple damage sets. This retrieves the 'active' damage set,
e.g. if you're using a bastard sword and choosing the 2h damage set.")
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

(defun make-weapon (name description &rest damage-options)
  (make-instance 'item
                 :name name
                 :description description
                 :weapon-stats (make-instance 'weapon-stats
                                              :damage-sets damage-options)))

(defparameter *weapons* `((dagger . ,(make-weapon "dagger"
                                                  "A dagger! It's pointy!"
                                                  '1h
                                                  '(:slice 2
                                                    :pierce 6)))
                          (mace   . ,(make-weapon "mace"
                                                  "A mace! It's blunty!"
                                                  '1h
                                                  '(:blunt 10
                                                    :pierce 2)))
                          (mace   . ,(make-weapon "bastard sword"
                                                  "A bastard sword! It's a bastard!"
                                                  '2h
                                                  '(:blunt 3
                                                    :slice 10)
                                                  '1h
                                                  '(:blunt 2
                                                    :slice 7)))))

(defun select-weapon (weapon)
  (assocdr weapon *weapons*))
