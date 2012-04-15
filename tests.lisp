;; From Practical Common Lisp, ch. 9
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)


(defun test-body-part ()
  (let* ((body-part-prototype (make-body-part-prototype 'test-head :targeting-weight 10))
         (body-part (make-body-part body-part-prototype "head")))
    (modify-damage body-part 'slice 20)
    (modify-damage body-part 'blunt 90)
    (describe-game-object body-part)))

(defun test-body ()
  (let ((body (make-body 'humanoid)))
    (modify-damage (body-part body "neck")        'slice  20)
    (modify-damage (body-part body "left eye")    'pierce 90)
    (modify-damage (body-part body "left eye")    'blunt  90)
    (modify-damage (body-part body "right thigh") 'blunt 30)
    (describe-game-object body)
    (look body)))

(defun test-attack ()
  (let ((attacker (make-body 'humanoid))
        (defender (make-body 'humanoid))
        (weapon   (select-item "dagger")))
    (attack attacker defender weapon)
    (look defender)))

(setq giant  (make-body 'humanoid))
(setq hobbit (make-body 'humanoid))
(setq dagger (select-item "dagger"))
