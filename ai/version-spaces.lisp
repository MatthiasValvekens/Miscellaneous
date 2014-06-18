(load "tree.lisp")
(defstruct (feature (:constructor make-feature (name hierarchy)))
  (name)
  (hierarchy nil :type tree-node))
(defun range (a b)
  (loop for i from a to b collect i))
(defun matches-type (sample type)
  (not (null (df sample type))))
(defun is-subtype-p (sub super)
  (matches-type (tree-node-value sub) super))
(defun accepts-p (hypothesis sample)
  (every (lambda (x) (matches-type (car x) (car (cdr x))))  
	 (mapcar #'cons sample hypothesis)))
(defun substitute-nth (val n list)
  (loop for i from 0 for j in list collect (if (= i n) val j)))
;discard all elements until condition is satisfied
(defun trim-until (test lst)
  (if (funcall test (car lst)) lst
      (trim-until test (cdr lst))))
(defun find-overly-strict-feature (hypothesis sample)
  (find-if
   (lambda (x) (not (matches-type (nth 1 x) (car (nth 2 x)))))
   (mapcar #'list
	   (range 0 (- (length hypothesis) 1))
	   sample
	   hypothesis)))
(defun translate-hypothesis (hypothesis features)
  (loop for arg in (mapcar #'cons hypothesis features) collect
       (if (car arg) (df (car arg) (feature-hierarchy (cdr arg))))))
;fix ONE bad feature in the hypothesis
;hypotheses are passed as tuples of paths of tree nodes
(defun generalize-to (hypothesis sample)
  (if (accepts-p hypothesis sample) hypothesis
      (destructuring-bind (pos samp feat)
	  (find-overly-strict-feature hypothesis sample)
	(generalize-to (substitute-nth ;replace the one bad position with a sufficiently generalized version, then recurse
			(trim-until ;ordering starts with most specialized
			 (lambda (x) (matches-type samp x))
			 feat)
			pos hypothesis) sample))))