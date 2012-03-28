(defparameter *body-part-prototypes* make-hash-table)
(defun make-body-part-prototype (name &key length base-body-part targeting-weight damage-descriptions)
  (setf (gethash name *body-part-prototypes*)
        (make-instance 'body-part-prototype
                       :name names
                       :length length
                       :base-body-part base-body-part
                       :targeting-weight targeting-weight
                       :damage-descriptions damage-descriptions)))

(make-body-part-prototype 'eye
                          :length 3
                          :targeting-weight )
