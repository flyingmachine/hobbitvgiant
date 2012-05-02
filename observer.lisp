(defclass observable ()
  ((observers
    :initform (make-hash-table)
    :accessor observers)))

(defclass player (observable)
  ((health
    :initarg :health
    :initform 100
    :accessor health)

   (ap
    :initarg :ap
    :initform 20
    :accessor ap)))

(defclass game-room (observable)
  ((description
    :initarg :description
    :initform "It's a room"
    :accessor description)

   (events
    :initarg :events
    :accessor events)))

(defmacro observable-slots (classname &rest slotnames)
  `(progn
     ,@(mapcar (lambda (slotname)
                 `(defmethod (setf ,slotname) :after (val (instance ,classname))
                             (mapc (lambda (observer)
                                     (funcall observer val))
                                   (gethash ',slotname (observers instance)))))
               slotnames)))

(defmethod add-observer ((observed observable) slotname fn)
  (setf (gethash slotname (observers observed))
        (nconc (gethash slotname (observers observed)) (list fn))))


(defun room-subscribe-to-player (room player)
  (mapc (lambda (slotname)
          (add-observer player
                        slotname
                        (lambda (val)
                          (setf (events room) (list slotname val)))))
        '(health ap)))

(observable-slots player health ap)
(observable-slots game-room events)

(setf p1 (make-instance 'player))
(setf p2 (make-instance 'player))

(setf r1 (make-instance 'game-room))
(setf r2 (make-instance 'game-room))

(room-subscribe-to-player r1 p1)
(room-subscribe-to-player r1 p2)
