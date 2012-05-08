(in-package :hobbitvgiant)

;; ---
;; body layers
;; ---
(defclass body-layer ()
  ((body
    :documentation "The body this belongs to."
    :initarg :body
    :accessor body)
   
   (body-parts
    :documentation "The body parts for this layer"
    :initarg :body-parts
    :reader body-parts)

   (height
    :initarg :height
    :reader  height)

   (base
    :documentation "Layer it sits on top of"
    :initarg :base
    :reader  base)))

(defun make-body-layer (body body-parts height base)
  (let1 layer (make-instance 'body-layer
                             :body body
                             :body-parts body-parts
                             :height height
                             :base base)
    (mapc (lambda (body-part)
            (setf (body-layer body-part) layer))
          body-parts)
    
    layer))
