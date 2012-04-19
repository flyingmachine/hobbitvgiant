(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun symb-up (&rest args)
  (values (intern (string-upcase (apply #'mkstr args)))))

(defun func (&rest args)
  (symbol-function (apply  #'symb-up args)))


(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


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

(defun alist-values (alist)
  (mapcar #'cdr alist))

(defun alist-keys (alist)
  (mapcar #'car alist))

(defun assocar (key alist)
  (car (assoc key alist)))

(defun assocdr (key alist)
  (cdr (assoc key alist)))

(defun merge-alists (key-source value-source)
  (mapcar (lambda (key)
            (cons key (cdr (assoc key value-source))))
          (alist-keys key-source)))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))
