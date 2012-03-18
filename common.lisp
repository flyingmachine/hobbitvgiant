(defclass damage ()
  ((slice
    :initarg :slice
    :accessor slice-damage-for
    :initform 0)
   (blunt
    :initarg :blunt
    :accessor blunt-damage-for
    :initform 0)
   (pierce
    :initarg :pierce
    :accessor pierce-damage-for
    :initform 0)
   (fire
    :initarg :fire
    :accessor fire-damage-for
    :initform 0)
   (ice
    :initarg :ice
    :accessor ice-damage-for
    :initform 0)))

(defmacro set-damage (damage damage-type val)
  `(setf (,(symb-up damage-type '-damage-for) ,damage) ,val))
