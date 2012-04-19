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

(defun make-body-layer-template (name height &rest prototype-pairings)
  (list name height (mapcan (lambda (pairing)
                              (if (consp pairing)
                                  (mapcar (lambda (name)
                                            (cons (car pairing) name))
                                          (cdr pairing))
                                  (list (cons pairing (mkstr pairing)))))
                            prototype-pairings)))

(make-body-template 'humanoid
                    (make-body-layer-template 'foot 13
                     '(foot "left foot" "right foot"))

                    (make-body-layer-template 'lower-leg 39
                     '(lower-leg "left lower leg" "right lower leg")
                     '(achilles "left achilles" "right achilles"))

                    (make-body-layer-template 'upper-leg 52
                     '(femoral-artery "left femoral artery" "right femoral artery")
                     '(thigh "left thight" "right thigh"))

                    (make-body-layer-template 'pelvis 13
                     'junk
                     'ass)

                    (make-body-layer-template 'torso 65
                     '(shoulder "left shoulder" "right shoulder")
                     '(upper-arm "left upper arm" "right upper arm")
                     '(forearm "left forearm" "right forearm")
                     'chest
                     'abdomen
                     'back
                     '(kidney "left kidney" "right kidney"))

                    (make-body-layer-template 'head 26
                     '(eye "left eye" "right eye")
                     'head
                     'neck
                     'jugular
                     'wind-pipe))
