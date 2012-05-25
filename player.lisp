(in-package :hobbitvgiant)
(defclass player (game-object named-object)
  ((body
    :initarg  :body
    :accessor body)

   (notification-handlers
    :initarg :notification-handlers
    :accessor notification-handlers
    :initform nil))
  (:metaclass observable))

(defmethod initialize-instance :after ((player player) &key)
  (setf (player (body player)) player))

(defun notify-player (player channel event)
  (mapc (lambda (notification-handler)
          (funcall notification-handler player channel event))
        (notification-handlers player)))
