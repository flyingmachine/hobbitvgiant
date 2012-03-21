(defparameter *damage-types* '(slice blunt pierce fire ice))

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

(defun damage-for (damage damage-type)
  (funcall (func damage-type '-damage-for) damage))

(defsetf damage-for (damage damage-type) (val)
  `(setf-damage ,damage ,damage-type ,val))

(defmacro setf-damage (damage damage-type val)
  `(setf (,(symb-up damage-type '-damage-for) ,damage) ,val))

(defmacro incf-damage (damage damage-type val)
  `(incf (,(symb-up damage-type '-damage-for) ,damage) ,val))

(defmacro decf-damage (damage damage-type val)
  `(decf (,(symb-up damage-type '-damage-for) ,damage) ,val))
