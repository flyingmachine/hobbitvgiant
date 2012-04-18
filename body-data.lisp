;; TODO read this data from CSV files
(defparameter *body-part-prototypes* (make-hash-table))
(defparameter *body-templates* (make-hash-table))

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

(defun make-body-template (name &rest layers)
  (setf (gethash name *body-templates*) layers))

(make-body-template 'humanoid
                    '(head
                      26
                      '((eye . "left eye")
                        (eye . "right eye")
                        (head)
                        (neck)
                        (jugular)
                        (wind-pipe)))
                    '(torso
                      65
                      '((shoulder . "left shoulder")
                        (shoulder . "right shoulder")
                        (upper-arm . "left upper arm")
                        (upper-arm . "right upper arm")
                        (forearm . "left forearm")
                        (forearm . "right forearm")
                        (chest)
                        (abdomen)
                        (back)
                        (kidney . "left kidney")
                        (kidney . "right kidney")))
                    '(pelvis
                      13
                      '((junk)
                        (ass)))
                    '(upper-leg
                      52
                      '('(femoral-artery . "left femoral artery")
                        '(femoral-artery . "right femoral artery")
                        '(thigh . "left thigh")
                        '(thigh . "right thigh")))
                    '(lower-leg
                      39
                      '('(lower-leg . "left lower leg")
                        '(lower-leg . "right lower leg")
                        '(achilles . "left achilles")
                        '(achilles . "right achilles")))
                    '(foot
                      13
                      '('(foot . "left foot")
                        '(foot . "right foot"))))
