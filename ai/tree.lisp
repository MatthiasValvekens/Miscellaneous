(defun make-counter (&optional (start-val 0))
  (let ((cur-count start-val))
    (lambda () (incf cur-count))))
(defun string-cast (thing)
  (if (stringp thing)
      thing
      (write-to-string thing)))
(defstruct (tree-node (:constructor make-tree-node (value)))
  (value)
  (children nil :type list))
(defun tree-node-add-children (parent children)
  (setf (tree-node-children parent) (append (tree-node-children parent) children))
  parent)
(defun tree-node-dot-body-inner (node my-index counter translator)
  (let ((result-string ""))
    (loop for c in (tree-node-children node) do
	 (let ((child-index (funcall counter)))
	   (setq result-string
		 (concatenate 'string 
			      result-string
			      (string-cast child-index)
			      "[label=\""
			      (string-cast (funcall translator c))
			      "\"];"
			      (string #\newline)
			      (string-cast my-index) 
			      "->" 
			      (string-cast child-index) 
			      ";"
			      (string #\newline)
			      (tree-node-dot-body-inner c child-index counter translator)))))
    result-string))
(defun tree-node-dot-body (node my-index counter &optional (translator (lambda (x) (tree-node-value x))))
  (concatenate 'string
	       (string-cast my-index)
	       "[label=\""
	       (string-cast (funcall translator node))
	       "\"];"
	       (string #\newline)
	       (tree-node-dot-body-inner node my-index counter translator)))
(defun tree-nodes-to-dot (nodes &key (graphname "") (translator (lambda (x) (tree-node-value x)))) ;trivial todo: fix start symbol label
  (concatenate 'string 
	       "digraph {"
	       (string #\newline)
	       "charset=\"UTF-8\";"
	       (string #\newline)
	       "graph [fontname=\"MS Mincho\", labelloc=\"t\", label=\""
	       graphname
	       "\"];"
	       (string #\newline)
	       (let ((ctr (make-counter)))
		 (apply #'concatenate (cons 'string (loop for node in nodes collect (tree-node-dot-body node (funcall ctr) ctr translator)))))
	       "}"))
;this implementation is pretty much bound to be better than whatever I'd come up with
;modified to preserve paths
(defun extend-path (path)
  (loop for c in (tree-node-children (car path))
       collect (cons c path)))
(defun df-iter (value nodelist)
  (cond ((null nodelist) nil)
	 ((string= (tree-node-value (car (car nodelist))) value) (car nodelist))
	  (t (df-iter value (nconc (extend-path (car nodelist)) 
				  (cdr nodelist))))))
(defun df (value startnode)
  (df-iter value (list (list startnode))))
(provide "tree")