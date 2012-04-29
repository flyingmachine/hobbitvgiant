;; Figure out who's in the engagement?
(defun begin-engagement ())

(defun turn (engagees)
  (if (engagement-ended engagees)
      nil
      (let1 current (car engagees)
        (if (dead? current)
            (turn (cdr engagees))
            (progn
              (do ((ap (max-ap current) (- ap (combat-action current engagees)))
                   (i 0 (1+ i)))
                  ((or (<= ap 0) (> i 10))))
              (turn (append1 (cdr engagees) current)))))))

;; For now, end engagement when just one engagee is left
(defun engagement-ended (engagees)
  (not (cdr engagees)))
