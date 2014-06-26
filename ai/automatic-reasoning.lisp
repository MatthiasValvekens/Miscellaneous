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
(defun setp (lst &optional (forbidden nil))
  (cond ((null lst) t)
	((member (car lst) forbidden) nil)
	(t (setp (cdr lst) (cons (car lst) forbidden)))))
;Martelli-Montanari unification
(defun find-mgu (lit-a lit-b vars)
  (funcall
   (y (lambda (me)
	(lambda (u)
	  (if (and (every (lambda (x) (member (car x) vars)) u)
		   (let ((subable-vars (mapcar #'car u)))
		     (and (setp subable-vars)
			  (every (lambda (x) (every (lambda (var) (not (containsp var (cdr x)))) subable-vars)) u))))
	      u		  ;end when only proper substitutions are left
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
				(nconc (mapcar #'cons arglist-a arglist-b)
				       (cdr u))))) ;extract

			 (t (reverse (cons (car u) (reverse (cdr u))))))))))) ;move useless stuff to back
   (list (cons lit-a lit-b))))
(defun apply-subs* (expr subs*)
  (if (null subs*)
      expr
      (apply-subs* (sub-all* (caar subs*) (cdar subs*) expr) (cdr subs*))))
; effects a sequence of substitutions
(defun apply-subs (explist subs)
  (if (null explist) 
      nil
      (cons (apply-subs* (car explist) subs) (apply-subs (cdr explist) subs))))
	     
(defun unify (lit-a lit-b vars)
  (let ((mgu (find-mgu lit-a lit-b vars)))
    (if mgu 
	(apply-subs* lit-a mgu)
	nil)))
(defun find-binding (var bindings)
  (cond ((eq bindings nil) nil)
	((eq (caar bindings) var) (car bindings))
	(t (find-binding var (cdr bindings)))))
;adapted from paradigms of AI programming, but with less frills
;also avoids ambiguous assignments
(defun pattern-match* (vars pattern input bindings)
  (cond ((eq bindings 'fail) 'fail)
	((member pattern vars)
	  (let ((binding (find-binding pattern bindings)))
		(if (or (eq binding nil) (equal input (cdr binding)))
		    (cons (cons pattern input) bindings)
		    nil)))
	((equal pattern  input) bindings)
	((and (consp pattern) (consp input))
	 (pattern-match* vars (cdr pattern) (cdr input)
			(pattern-match* vars (car pattern) (car input)
				       bindings)))
	(t 'fail)))
(defun pattern-match (vars pattern input)
  (let ((mat (pattern-match* vars pattern input nil)))
    (if (eq mat 'fail) nil mat)))
(defun transform (vars in-pattern out-pattern input)
  (apply-subs* out-pattern (pattern-match vars in-pattern input)))