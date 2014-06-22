;a literal takes the form (fun-symb &rest arglist) (i.e. an S-expression without leading lambda's)
;a clause is a list of ORed literals
;an expression is a list of ANDed clauses
(defun y (fun)
  ((lambda (x) (funcall x x))
   (lambda (y)
     (funcall fun (lambda (&rest args)
		    (apply (funcall y y) args))))))

(defun list-symbols (expr)
  (cond ((null expr) nil)
	((symbolp expr) (list expr))
	((consp expr) (append (list-symbols (car expr)) (list-symbols (cdr expr))))))
(defun litp (expr)
  (symbolp (car expr)))
;extract all function symbols from an expression (or an arbitrarily nested list of expressions)
;functions are symbols occuring as the first element in a list
;predicates are just special functions D->{0,1}
(defun list-functions (expr)
  (funcall 
   (y (lambda (me)
	(lambda (expr* is-arglist)
	  (cond ((null expr*) nil)
		((consp expr*) 
		 (if (and (not is-arglist) (litp expr*))
		     (cons (car expr*) (funcall me (cdr expr*) t))
		     (append (funcall me (car expr*) nil) (funcall me (cdr expr*) is-arglist))))))))
      expr nil))
(defun containsp (symbol expr)
  (member symbol (list-symbols expr))) ;we can do better, but this is the simplest solution

(defun sub-all* (varsym expr target)
  (if (symbolp target)
      (if (eq target varsym) expr target)
      (loop for s in target collect
	   (if (symbolp s)
	       (if (eq s varsym) expr s)
	       (sub-all* varsym expr s)))))

(defun sub-all (varsym expr subbable)
  (if (null subbable) (list (cons varsym expr))
      (cons (cons (sub-all* varsym expr (caar subbable)) (sub-all* varsym expr (cdar subbable))) (sub-all varsym expr (cdr subbable)))))
(defun effect-substitution (u varsym expr)
    (sub-all varsym expr u))

;Martelli-Montanari unification
(defun find-mgu (lit-a lit-b vars)
  (funcall
   (y (lambda (me)
	(lambda (u)
	  (if (every (lambda (x) (member (car x) vars)) u)
	      u			 ;end when only substitutions are left
	      (funcall me	   
		       (cond 
			 ((and (not (member (caar u) vars)) 
			       (member (cdar u) vars)) ;s/t with s nonvariable and t variable should become t/s
			  (cons (cons (cdar u) (caar u)) (cdr u)))
			 ((and (member (caar u) vars) (eq (caar u) (cdar u)))
			  (cdr u))	;s/s is redundant
			 ((member (caar u) vars)
			  (let ((varsym (caar u))
				(expr (cdar u)))
			    (if (containsp varsym expr) nil ;self-referential substitution. Error
				(effect-substitution (cdr u) varsym expr))))
			 ((and (litp (caar u)) (litp (cdar u)))
			  (let ((fun-a (caaar u))
				(fun-b (cadar u))
				(arglist-a (cdaar u))
				(arglist-b (cddar u)))
			    (if (or (not (eq fun-a fun-b))
				    (not (= (length arglist-a)
					    (length arglist-b))))
				nil ;non-matching function substitution. Error 
				(append (cdr u)
					(mapcar #'cons arglist-a arglist-b))))) ;extract

			 (t (reverse (cons (car u) (reverse (cdr u))))))))))) ;move useless stuff to back
   (list (cons lit-a lit-b))))
		   