(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun symb-up (&rest args)
  (values (intern (string-upcase (apply #'mkstr args)))))

(defun func (&rest args)
  (symbol-function (apply  #'symb-up args)))
