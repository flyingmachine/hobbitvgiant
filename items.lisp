(in-package :hobbitvgiant)

;; eventually add support for enchantments
;; would like damage-sets to be a alist
;;
;; Each weapon has weapon stats, which can contain multiple damage
;; sets. For example, a throwing knife could have melee and thrown
;; damage sets.
;;
;; Each weapon also has armor stats. For example, a shield can be worn
;; but can also be used to bash people. Need to think about how to
;; support this.
;;
;; TODO does balance change when switching 1h/2h?
(defclass item (game-object named-object)
  ((weapon-stats
    :initarg :weapon-stats
    :reader weapon-stats)
   (description
    :initarg :description
    :accessor description)
   (item-length
    :initarg :item-length
    :reader  item-length)
   (weight
    :initarg  :weight
    :initform 1
    :reader   weight)
   (weight-capacity
    :documentation "If this is above 1, it can act as a container"
    :initarg  :weight-capacity
    :initform 0
    :reader weight-capacity)
   (item-length-capacity
    :documentation "A purse can't contain a halberd"
    :initarg :item-length-capacity
    :initform 0
    :reader item-length-capacity)
   (worth
    :documentation "How much money it's worth"
    :initarg :worth
    :initform 0
    :reader   worth)))

(defclass weapon-stats ()
  ((damage-sets
    :initarg :damage-sets
    :reader  damage-sets)
   (active-damage-set
    :accessor active-damage-set
    :initform '1h)))

(defgeneric active-damage-set (item)
  (:documentation "Weapons can have multiple damage sets. This retrieves the 'active' damage set,
e.g. if you're using a bastard sword and choosing the 2h damage set."))

(defmethod active-damage-set ((weapon item))
  (with-accessors ((weapon-stats weapon-stats)) weapon
    (gethash (active-damage-set weapon-stats) (damage-sets weapon-stats))))

;; This creates true damage sets out of the lists used in make-weapon
(defmethod initialize-instance :after ((weapon-stats weapon-stats) &key)
  (let ((dsets (make-hash-table)))
    (mapc (lambda (pair)
            (setf (gethash (car pair) dsets) (apply #'make-damage 0 (cdr pair))))
          (pairs (slot-value weapon-stats 'damage-sets)))
    (setf (slot-value weapon-stats 'damage-sets) dsets)))

(defvar *items* (make-hash-table :test 'equal))

;; Might be nice to make this a macro so that I don't have to put
;; single quotes in the weapon definition
(defun make-weapon (&key name description weight item-length damage-options)
  (setf (gethash name *items*) (make-instance 'item
                                              :name name
                                              :description description
                                              :weight weight
                                              :item-length item-length
                                              :weapon-stats (make-instance 'weapon-stats
                                                                           :damage-sets damage-options))))

(make-weapon :name "dagger"
             :description "A dagger! It's pointy!"
             :weight 450
             :item-length 50
             :damage-options '(1h (:slice 2 :pierce 6)))
(make-weapon :name "mace"
             :description "A mace! It's blunty!"
             :weight 1000
             :item-length 85
             :damage-options '(1h (:blunt 10 :pierce 2)))
(make-weapon :name "bastard sword"
             :description "A bastard sword! It's a bastard!"
             :weight 2270
             :item-length 100
             :damage-options '(2h (:blunt 3 :slice 10)
                               1h (:blunt 2 :slice 7)))
(make-weapon :name "flaming skull lantern"
             :description "A lantern containing the flaming skulll of a dead mage."
             :weight 1100
             :item-length 85
             :damage-options '(1h (:blunt 5 :fire  10)))

(defun select-item (item)
  (gethash item *items*))
