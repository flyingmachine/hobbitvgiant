(in-package :hobbitvgiant)
;;; observer

;;; Copyright (c) 2008 Olaf Ritter von Ruppert

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(defclass observable (standard-class)
  ())

(defmethod validate-superclass ((class observable) (superclass standard-class))
  t)

(defmethod compute-slots ((class observable))
  (cons (make-instance 'standard-effective-slot-definition
                       :name 'observers
                       :initform '(make-hash-table)
                       :initfunction #'(lambda () (make-hash-table)))
        (call-next-method)))

(defmethod (setf slot-value-using-class) :around
    (new (observable observable) instance slot)
  (let ((slot-name (slot-definition-name slot)))
    (if (slot-boundp instance slot-name)
        (let ((old (slot-value instance slot-name)))
          (multiple-value-prog1 (call-next-method)
            (dolist (observer (observers instance slot-name))
              (funcall observer new old))))
        (call-next-method))))

(defun observers (instance slot-name)
  (if-let (h (gethash slot-name (slot-value instance 'observers)))
       (hash-values h)
       (list)))

(defmacro add-observer (fn instance slot-name observer-name)
  (let ((observer-hash-for-slot `(gethash ,slot-name (slot-value ,instance 'observers))))
    `(progn (when (null ,observer-hash-for-slot)
              (setf ,observer-hash-for-slot (make-hash-table)))
            (setf (gethash ,observer-name ,observer-hash-for-slot) ,fn))))

(defun remove-observer (instance slot-name observer-name)
  (remhash observer-name (gethash slot-name (slot-value instance 'observers))))

(defmacro observe ((instance slot-name observer-name &optional new old) &body body)
  (let ((new (or new (gensym)))
        (old (or old (gensym))))
    `(add-observer (lambda (&optional ,new ,old)
                     (declare (ignorable ,new ,old))
                     ,@body)
                   ,instance ,slot-name ,observer-name)))

(defmacro observe-each ((instances slot-name observer-name &optional new old) &body body)
  (let ((new  (or new (gensym)))
        (old  (or old (gensym)))
        (ivar (gensym)))
    `(dolist (,ivar ,instances)
       (add-observer (lambda (&optional ,new ,old)
                       (declare (ignorable ,new ,old))
                       ,@body)
                     ,ivar ,slot-name ,observer-name))))
