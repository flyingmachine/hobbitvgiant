(in-package :hobbitvgiant)
(defclass game-object ()
  ((id
    :initarg :id
    :initform (gensym)
    :reader id)))

(defclass named-object ()
  ((name
    :initarg :name
    :reader name)))
