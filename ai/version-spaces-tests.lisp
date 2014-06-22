(setq *feat1* (make-feature 'creature-type (tree-node-add-children (make-tree-node 'creature) (list (make-tree-node 'plant) (tree-node-add-children (make-tree-node 'animal) (list (make-tree-node 'human) (make-tree-node 'platypus)))))))
(setq *feat2* (make-feature 'creature-status (tree-node-add-children (make-tree-node 'status) (list (make-tree-node 'extinct) (tree-node-add-children (make-tree-node 'not-extinct) (list (make-tree-node 'awesome) (make-tree-node 'not-awesome)))))))
(defun translator (h) (translate-hypothesis h (list *feat1* *feat2*)))

(princ (tree-nodes-to-dot (loop for x in (find-boundary-nodes (list (feature-hierarchy *feat1*)) (lambda (x) (not (matches-type 'animal x)))) collect (car x))))


(readable-hypothesis-list (specialize-to (translate-hypothesis '(creature status) (list *feat1* *feat2*)) '(human not-awesome)))

(generalizes-p (translator '(creature not-extinct)) (translator '(human awesome)))
(generalizes-p (translator '(creature extinct)) (translator '(human awesome)))
(setq *trainer* (make-trainable-judge (list *feat1* *feat2*)))
(funcall (car *trainer*)
	 (make-train-data 
	  (list
	   '(human awesome)
	   '(animal awesome))
	  (list
	   '(human not-awesome)
	   '(plant extinct))))
(print (funcall (cadr *trainer*) '(plant extinct)))
(print (funcall (third *trainer*)))

(has-strict-generalization-in-p (translator '(human awesome)) (list (translator '(human not-awesome))(translator '(human awesome))))