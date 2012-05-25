(defun json-out (obj)
  (with-output-to-string (*standard-output*)
    (json:encode-json obj)))
