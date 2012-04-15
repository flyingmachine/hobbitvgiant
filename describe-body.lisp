;; The damage description data structure assumes that we'll never want
;; to completely remove a base data structure
;;
;; Gist is that you create an alist by appending base descriptions to
;; custom ones. Then sort the keys and use the sorted keys to find
;; which description to use. Then use assoc to get that description
;;
;; If I do end up having item damage descriptions, will probably use
;; the same code
;;
;; Returns a list of all descriptions that apply based on each kind of
;; damage done
(defmethod describe-damage ((body-part body-part))
  (let ((descriptions (damage-descriptions body-part))
        (body-part-damage (damage-received body-part)))
    (remove nil (mapcar (lambda (damage-type)
                          (let ((descriptions-for-type (damage-for descriptions damage-type)))
                            (cdr (assoc (find (damage-for body-part-damage damage-type)
                                                 (sort (mapcar (lambda (desc) (car desc)) descriptions-for-type) #'>) ;; sort keys descending
                                                 :test (lambda (damage-received trigger-point)
                                                         (>= damage-received trigger-point))) descriptions-for-type))))
                        *damage-types*))))

;; Does it make sense tao have a generic describe method for like
;; everything in the game?
(defgeneric describe-game-object (game-object)
  (:documentation "Compiles the final description for an object. Format:
[[set one first line [indented [indented more] indented]] [set two first line]]"))

(defmethod describe-game-object ((body-part body-part))
  (let ((descriptions (describe-damage body-part)))
    (when descriptions
      (list (let ((preamble (mkstr "Its " (name body-part) " is")))
              (if (cdr descriptions)
                  (list preamble descriptions)
                  (list (mkstr preamble " " (first descriptions)))))))))


(defmethod describe-game-object ((body body))
  (remove nil (mapcan (lambda (body-part) (describe-game-object body-part)) (body-parts body))))


(defun look (game-object)
  (let ((description (describe-game-object game-object)))
    (labels ((formatted-output (l level)
               (mapc (lambda (r)
                       (if (typep r 'list)
                           (formatted-output r (1+ level))
                           (format t "~v{ ~}~a~%" level '(foo) r))) l)))
      (mapc (lambda (d) (formatted-output d 0)) description))))
