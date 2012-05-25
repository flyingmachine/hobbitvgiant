(in-package :hobbitvgiant)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun last1 (lst)
  (car (last lst)))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun symb-up (&rest args)
  (values (intern (string-upcase (apply #'mkstr args)))))

(defun func (&rest args)
  (symbol-function (apply  #'symb-up args)))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x) 
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun append1 (lst obj) (append lst (list obj)))

;; from land of lisp
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                 ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                     (if tail
                         (f (cdr tail) (cons (cons head (car tail)) acc))
                       (reverse acc))
                     (reverse acc))))
    (f lst nil)))

(defun pairsr (&rest lst)
  (pairs lst))

(defmacro keyed-pair (attribute object)
  `(cons ',attribute (,attribute ,object)))

(defun alist-values (alist)
  (mapcar #'cdr alist))

(defun alist-keys (alist)
  (mapcar #'car alist))

(defun flatten-alist (alist)
  (let ((flat nil))
    (mapc (lambda (pair)
            (push (car pair) flat)
            (push (cdr pair) flat))
          alist)
    (nreverse flat)))

(defun assocar (key alist)
  (car (assoc key alist)))

(defun assocdr (key alist)
  (cdr (assoc key alist)))

(defun merge-alists (key-source value-source)
  (mapcar (lambda (key)
            (cons key (cdr (assoc key value-source))))
          (alist-keys key-source)))

(defun make-hash-from-pairs (pairs)
  (let1 h (make-hash-table)
    (mapc (lambda (pair)
            (setf (gethash (first pair) h) (if (consp (cdr pair)) (second pair) (cdr pair))))
          pairs)
    h))

(defun make-hash-from-pairsr (&rest pairs)
  (make-hash-from-pairs (pairs pairs)))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))
       
(defun hash-values (hash-table)
  (loop for key being the hash-values of hash-table collect key))

(defmacro if-let ((varname form) &body body)
  `(let ((,varname ,form))
     (if ,varname
         ,@body)))

;; Given a collection, return an alist where the car is the member and
;; the cdr is the supplied function called on the member
(defun alist-by-func (collection func)
  (mapcar (lambda (member)
            (cons member (funcall func member)))
          collection))

(defun maphashv (function-designator hash-table)
  (let ((new-hash (make-hash-table)))
    (maphash (lambda (k v)
               (setf (gethash k new-hash) (funcall function-designator v)))
             hash-table)
    new-hash))

;; modify the values of hash a by applying a function to values from
;; hash a and hash b
(defun mergehash (hash-a hash-b fn)
  (maphash (lambda (k v)
             (setf (gethash k hash-a) (funcall fn v (gethash k hash-b))))
           hash-a)
  hash-a)
