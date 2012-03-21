(defparameter *damage-types* '(slice blunt pierce fire ice))

;; create a damage hash
(defun make-damage (default &key slice blunt pierce fire ice)
  (macrolet ((setter ()
               `(progn
                  ,@(mapcar (lambda (dtype)
                              `(setf (gethash ',dtype d) (or ,dtype default))) *damage-types*))))
    (let ((d (make-hash-table)))
      (setter)
      d)))

(defun damage-for (damage damage-type)
  (gethash damage-type damage))

(defun set-damage-for (damage damage-type val)
  (setf (gethash damage-type damage) val))

(defsetf damage-for set-damage-for)

(defmacro setf-damage (damage damage-type val)
  `(setf (,(symb-up damage-type '-damage-for) ,damage) ,val))

(defmacro incf-damage (damage damage-type val)
  `(incf (,(symb-up damage-type '-damage-for) ,damage) ,val))

(defmacro decf-damage (damage damage-type val)
  `(decf (,(symb-up damage-type '-damage-for) ,damage) ,val))
