;python-like itertools
(defun enumerate (lst &optional (ctr 0))
  (if (null lst) nil
      (cons (cons ctr (car lst)) (enumerate (cdr lst) (+ ctr 1)))))