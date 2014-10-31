#!/usr/bin/lisp

;;; really simple (and highly inefficient) contact managing tool

(defvar *db-file* "~/.contacts.db")
(defvar *contacts-db* nil)
(defun y (fun)
  ((lambda (x) (funcall x x))
   (lambda (y)
     (funcall fun (lambda (&rest args)
		    (apply (funcall y y) args))))))
(defun load-db ()
  (with-open-file (in *db-file* :direction :input)
    (with-standard-io-syntax (setq *contacts-db* (read in)))))
(defun save-db ()
  (with-open-file (out *db-file* :if-does-not-exist :create :direction :output)
    (print *contacts-db* out)))
(defun contact-exists-p (name)
  (not (null (fetch-property name :name))))
(defun add-contact (record) 
  (if (getf record :name)
      (progn
	(push record *contacts-db*)
	record)))
(defun flatten-pairs (fields)
  (if fields
      (destructuring-bind (field-name field-value)
	  (car fields)
	(cons field-name (cons field-value (flatten-pairs (cdr fields)))))))
(defun fetch-property (name property)
  (if *contacts-db* 
      (funcall
       (y (lambda (me)
	    (lambda (list)
	      (if (equal name (getf (car list) :name))
		  (getf (car list) property)
		  (funcall me (cdr list))))))
       *contacts-db*)))
(defun not-empty-p (str)
  (not (equal str "")))
(defun prompt-property (property &optional (verifyfun #'not-empty-p) (parsefun #'identity))
  (format *query-io* "~a: " property)
  (force-output *query-io*)
  (let ((input (read-line *query-io*)))
    (if (funcall verifyfun input)
	(list property (funcall parsefun input)))))
(defun prompt-contact ()
  (add-contact
    (nconc
     (prompt-property :name)
     (prompt-property :fullname)
     (prompt-property :email)
     (prompt-property :mobile)
     (prompt-property :address))))