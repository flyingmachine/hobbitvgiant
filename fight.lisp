(defparameter *player-health* 0)
(defparameter *giant-health* 0)

(defun fight ()
  (init-giant)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (giant-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (giant-dead))
    (show-player)
    (fresh-line)
    (player-attack)
    (fresh-line)
    (giant-attack)
    (fresh-line)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 100))

(defun init-giant ()
  (setf *giant-health* 300))

(defun player-dead ()
  (<= *player-health* 0))

(defun giant-dead ()
  (<= *giant-health* 0))

(defun player-attack ()
  (format t "You swing!")
  (decf *giant-health* 10))

(defun giant-attack ()
  (format t "The giant swings at you!")
  (decf *player-health* 45))

(defun show-player ()
  (format t "You have ~a health points." *player-health*))

