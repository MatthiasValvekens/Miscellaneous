(load "tree.lisp")
(load "itertools.lisp")
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
(defun trim-until (test lst &optional with-lookahead)
  (if (or (null lst) 
	  (if with-lookahead
	      (funcall test (car (cdr lst)))
	      (funcall test (car lst))))
      lst
      (trim-until test (cdr lst) with-lookahead)))
(defun do-find-spurious-feature (hypothesis sample test)
  (find-if
   test
   (zip-enumerate sample hypothesis)))
(defun find-spurious-feature (hypothesis sample match)
  (let ((basetest (lambda (x) (matches-type (nth 1 x) (car (nth 2 x))))))
    (if match
	(do-find-spurious-feature hypothesis sample basetest)
	(do-find-spurious-feature hypothesis sample (lambda (x) (not (funcall basetest x)))))))
(defun df-wrapper (value node &key root-first)
  (if root-first
      (reverse (df value node))
      (df value node)))
(defun translate-hypothesis (hypothesis features &key general-first)
  (loop for arg in (mapcar #'cons hypothesis features) collect
       (if (car arg) (df-wrapper (car arg) (feature-hierarchy (cdr arg)) :root-first general-first))))
;fix ONE bad feature in the hypothesis, then recurse
;hypotheses are passed as tuples of paths of tree nodes
;marginal generalisation is always unique in tree hierachies
(defun generalize-to (hypothesis sample)
  (if (accepts-p hypothesis sample) hypothesis
      (destructuring-bind (pos samp feat)
	  (find-spurious-feature hypothesis sample nil)
	(generalize-to (substitute-nth ;replace the one bad position with a sufficiently generalized version, then recurse
			(trim-until ;ordering starts with most specialized
			 (lambda (x) (matches-type samp x))
			 feat)
			pos hypothesis) sample))))
(defun find-boundary-nodes (path test)
  (if (funcall test (car path)) 
      (list path)
      (if (null (tree-node-children (car path)))
	  nil
	  (loop for child in (tree-node-children (car path)) nconc (find-boundary-nodes (cons child path) test)))))
;specializes the hypothesis until sample is rejected
;generate all
(defun specialize-to (hypothesis sample)
  (if (not (accepts-p hypothesis sample)) (list hypothesis)
      (destructuring-bind (pos samp feat)
	  (find-spurious-feature hypothesis sample t)
	(let ((matchtest (lambda (x) (not (matches-type samp x)))))
	  (loop for path in (find-boundary-nodes feat matchtest)
	       nconc (specialize-to (substitute-nth path pos hypothesis) sample))))))

(defstruct (train-data (:constructor make-train-data (accept reject)))
  (accept)
  (reject))
	   
(defun make-trainable-judge (features)
  (let ((S (list features )) ;<--- extract stuff
	(G nil))
    (list (lambda (t-data)
	    ;training code here
	    t)
	  (lambda (sample)
	    ;judging code computing the truth probability here
	    0)
	  )))