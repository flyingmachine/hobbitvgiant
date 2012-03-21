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

;; rename to alist?
(defun plist (&rest properties)
  (group properties 2))

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
