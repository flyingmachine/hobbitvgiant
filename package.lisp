;;;; package.lisp

(defpackage #:hobbitvgiant
  (:use common-lisp
        #+sbcl sb-mop
        #+clisp mop))
