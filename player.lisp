(in-package :hobbitvgiant)
(defclass player (game-object named-object)
  ((output-stream
    :initarg :output-stream
    :initform *standard-output*
    :accessor output-stream)

   (body
    :initarg  :body
    :accessor body))
  (:metaclass observable))

(defmethod initialize-instance :after ((player player) &key)
  (setf (player (body player)) player))

(defun notify-player (player channel event)
  (format (output-stream player)
          "~a has received a new event: ~a~%"
          (name player)
          (mkstr event)))
