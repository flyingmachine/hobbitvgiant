;; TODO
(defparameter *body-part-prototypes* make-hash-table)
(defparameter *body-templates* make-hash-table)
(defun make-body-part-prototype (name &key length base-body-part targeting-weight damage-descriptions)
  (setf (gethash name *body-part-prototypes*)
        (make-instance 'body-part-prototype
                       :name name
                       :targeting-weight targeting-weight
                       :damage-descriptions damage-descriptions)))

(make-body-part-prototype 'eye             :targeting-weight 1)
(make-body-part-prototype 'head            :targeting-weight 3)
(make-body-part-prototype 'neck            :targeting-weight 2)
(make-body-part-prototype 'jugular         :targeting-weight 1)
(make-body-part-prototype 'wind-pipe       :targeting-weight 1)
(make-body-part-prototype 'shoulder        :targeting-weight 3)
(make-body-part-prototype 'upper-arm       :targeting-weight 3)
(make-body-part-prototype 'forearm         :targeting-weight 3)
(make-body-part-prototype 'chest           :targeting-weight 10)
(make-body-part-prototype 'abdomen         :targeting-weight 10)
(make-body-part-prototype 'back            :targeting-weight 10)
(make-body-part-prototype 'kidney          :targeting-weight 2)
(make-body-part-prototype 'junk            :targeting-weight 2)
(make-body-part-prototype 'ass             :targeting-weight 4)
(make-body-part-prototype 'femoral-artery  :targeting-weight 1)
(make-body-part-prototype 'thigh           :targeting-weight 4)
(make-body-part-prototype 'lower-leg       :targeting-weight 3)
(make-body-part-prototype 'achilles        :targeting-weight 1)
(make-body-part-prototype 'foot            :targeting-weight 2)



(defun make-body-template (name &rest prototype-names)
  (setf (gethash name *body-templates*) prototype-names))

(make-body-template 'humanoid
                    '(eye . "left eye")
                    '(eye . "right eye")
                    'head
                    'neck
                    'jugular
                    'wind-pipe
                    '(shoulder . "left shoulder")
                    '(shoulder . "right shoulder")
                    '(upper-arm . "left upper arm")
                    '(upper-arm . "right upper arm")
                    '(forearm . "left forearm")
                    '(forearm . "right forearm")
                    'chest
                    'abdomen
                    'back
                    '(kidney . "left kidney")
                    '(kidney . "right kidney")
                    'junk
                    'ass            
                    '(femoral-artery . "left femoral artery")
                    '(femoral-artery . "right femoral artery")
                    '(thigh . "left thight")
                    '(thigh . "right thight")
                    '(lower-leg . "left lower leg")
                    '(lower-leg . "right lower leg")
                    '(achilles . "left achilles")
                    '(achilles . "right achilles")
                    '(foot . "left foot")
                    '(foot . "right foot"))
