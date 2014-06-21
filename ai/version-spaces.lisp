(load "tree.lisp")
(load "itertools.lisp")
(defstruct (feature (:constructor make-feature (name hierarchy)))
  (name)
  (hierarchy nil :type tree-node))

(defun matches-type (sample type)
  (not (null (df sample type))))
;checks if hyp-a is more general than hyp-b
(defun generalizes-p (hyp-a hyp-b)
  (if (null hyp-b) t
      (and (df (tree-node-value (caar hyp-b)) (caar hyp-a)) (generalizes-p (cdr hyp-a) (cdr hyp-b)))))
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
(defun readable-hypothesis-list (hyp-lst)
  (loop for hyp in hyp-lst collect
       (loop for feat in hyp collect (tree-node-value (car feat)))))
(defun pp-hyp-list (hyp-list)
  (print (readable-hypothesis-list hyp-list))
  hyp-list)
;fix ONE bad feature in the hypothesis, then recurse
;hypotheses are passed as tuples of paths of tree nodes
;marginal generalisation is always unique in tree hierachies
(defun generalize-to (hypothesis sample)
  (if (accepts-p hypothesis sample) (list hypothesis) ;matter of being consistent
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
(defun has-generalization-in-p (hyp lst)
  (cond ((null lst) nil)
	((generalizes-p (car lst) hyp) t)
	((has-generalization-in-p hyp (cdr lst)))))
(defun has-specialization-in-p (hyp lst)
  (cond ((null lst) nil)
	((generalizes-p hyp (car lst)) t)
	((has-specialization-in-p hyp (cdr lst)))))
(defun make-trainable-judge (features)
  (let ((S (list nil))			;<--- extract stuff
	(G (list (loop for feat in features collect
		      (list (feature-hierarchy feat))))))
    (list (lambda (t-data)
					;generalize S
	    (if (null (car S)) (setq S (list (translate-hypothesis (car (train-data-accept t-data)) features))))
	    (loop for sample in (train-data-accept t-data)
	       do (progn
		    (setf 
		     S
		     (append S
			     (delete-if 
			      (lambda (hyp) (or 
					     (not (has-generalization-in-p hyp G))
					     (has-generalization-in-p hyp S)))
			      (loop for hyp in S nconc (generalize-to hyp sample)))))
		    (setf ;prune bad hypotheses from G
		     G
		     (delete-if
		      (lambda (hyp) (not(accepts-p hyp sample)))
		      G))))
	    (loop for sample in (train-data-reject t-data)
	       do (progn
		    (setf 
		     G 
		     (append G 
			     (delete-if
			      (lambda (hyp) (or (not (has-specialization-in-p hyp S))
						(has-specialization-in-p hyp G)))
			      (loop for hyp in G nconc (specialize-to hyp sample)))))
		    (setf
		     S
		     (delete-if
		      (lambda (hyp) (accepts-p hyp sample))
		      S))))
	    t)
	  (lambda (sample)
					;judging code computing the truth probability
	    (/ (loop for hyp in (append S G) summing
		    (if (accepts-p hyp sample) 1 0)) (+ (length S) (length G))))
	  (lambda nil
	    (list (readable-hypothesis-list S) (readable-hypothesis-list G))))))