;; Parts:

(defparameter *asym-body-parts*
  '((head . 3)
    (left-eye . 1)
    (left-ear . 1)
    (mouth . 1)
    (nose . 1)
    (neck . 2)
    (jugular . 1)
    (wind-pipe . 1)
    (left-shoulder . 3)
    (left-upper-arm . 3)
    (chest . 10)
    (back . 10)
    (left-forearm . 3)
    (abdomen . 6)
    (left-kidney . 1)
    (left-hand . 2)
    (junk . 3)
    (ass . 4)
    (left-femoral-artery . 2)
    (left-knee . 2)
    (left-thigh . 4)
    (left-lower-leg . 3)
    (left-achilles . 1)
    (left-foot . 2)))

(defun symmetrize-body-parts (body-parts)
  (labels ((part-and-match (part acc)
             (let ((part-string (string-downcase (symbol-name (car part)))))
               (append acc (list part) (when (search "left" part-string) (list (cons (intern (string-upcase (concatenate 'string "right" (subseq part-string 4)))) (cdr part)))))))
           (add-matching-parts (parts acc)
             (if (null parts)
                 acc
                 (add-matching-parts (cdr parts) (part-and-match (car parts) acc)))))
    (add-matching-parts body-parts nil)))


(defun body-part-sum ()
  (reduce #'+ (symmetrize-body-parts *asym-body-parts*) :key #'cdr))

(defun hit-part (body-parts)
  (symmetrize-body-parts (body-parts)))
