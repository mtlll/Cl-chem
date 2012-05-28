(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :acid-base
  (:use :common-lisp
	:data
	:core)
  (:export acid
	   base
	   defacid
	   defbase
	   ph))

(in-package :acid-base)

(defclass acid-or-base (compound)
  ((strong-p
    :reader strong-p
    :initform nil)
   (constant
    :reader constant
    :initarg :constant)
   (conjugate-base
    :reader conjugate-base
    :initarg :conjugate-base)))

(defmethod initialize-instance :after ((thing acid-or-base) &key)
  (with-slots (strong-p constant) thing
    (unless constant
      (setf strong-p t))))

(defclass acid (acid-or-base) ())
(defclass base (acid-or-base) ())

(defun listify (lst)
  (cons 'list
	(mapcar (lambda (x) (cons 'list x))
		lst)))

(defmacro make-acid (acid constant conjugate-base &rest dissociation-steps)
  (format t "~a~a" dissociation-steps (rest dissociation-steps))
  `(make-instance 'acid :constant ,constant
		  :conjugate-base
		  ,(if dissociation-steps
		       `(make-acid ,conjugate-base ,(first (first dissociation-steps)) ,(second (first dissociation-steps))
				   ,@(rest dissociation-steps))
		       `(make-instance 'compound :elements ,(listify conjugate-base)))
		  :elements ,(listify acid)))

(defmacro defacid (name acid constant conjugate-base &rest dissociation-steps)
  `(defparameter ,name (make-acid ,acid ,constant ,conjugate-base ,@dissociation-steps)))

(defmacro defbase (name base constant conjugate-base)
  `(defparameter ,name (make-instance 'base :constant ,constant
				      :conjugate-base ,(listify conjugate-base)
				      :elements ,(listify base))))

(defparameter *hydronium* (make-instance 'compound :charge 1 :elements '(("H" 3 0) ("O" 1 0))))
(defparameter *hydroxide* (make-instance 'compound :charge -1 :elements '(("O" 1 0) ("H" 1 0))))

(defgeneric good-vs-evil (amount-of in good-guy bad-guy)) 
(defmethod good-vs-evil (amount-of (in solution) (good-guy compound) (bad-guy compound))
  (with-solutes ((good good-guy)
		 (evil bad-guy))
      in
    (let ((diff (- (+ good amount-of)
		   evil)))
      (cond ((plusp diff)
	     (setf good diff
		   evil 0))
	    ((zerop diff)
	     (setf good 0
		   evil 0)) 
	    (t (setf good 0
		     evil (abs diff))))
      good)))

(defmethod solve (amount-of (comp (eql *hydronium*)) (in solution) &key)
  (good-vs-evil amount-of in *hydronium* *hydroxide*)) 

(defmethod solve (amount-of (comp (eql *hydroxide*)) (in solution) &key)
  (good-vs-evil amount-of in *hydroxide* *hydronium*))

(defgeneric a-or-b-to-compound (a-or-b))
(defmethod a-or-b-to-compound ((a-or-b acid-or-base)) 
  (make-instance 'compound :elements (elements a-or-b)
		 :charge (charge a-or-b)))

(defun calculate-x (constant amount-of)
  (/ (+ (- constant)
	(sqrt (+ (expt constant 2)
		 (* 4 constant
		    amount-of))))
     2))

(defgeneric increase-ph (type amount-of comp in indicator))

(defmethod increase-ph ((type (eql 'strong)) amount-of (comp acid-or-base) (in solution) (indicator compound))
  (with-slots (conjugate-base) comp
    (solve amount-of indicator in)
    (solve amount-of conjugate-base in)))

(defmethod increase-ph ((type (eql 'weak)) amount-of (comp acid-or-base) (in solution) (indicator compound)) 
  (with-slots (constant conjugate-base) comp
    (let ((x (calculate-x constant amount-of)))
      (solve x indicator in)
      (solve x conjugate-base in)
      (solve (- amount-of x)
	     (a-or-b-to-compound comp)
	     in))))


(defmethod solve (amount-of (comp acid-or-base) (in solution) &key indicator)
  (if (strong-p comp)
      (increase-ph 'strong amount-of comp in indicator)
      (increase-ph 'weak amount-of comp in indicator)))

(defmethod solve (amount-of (comp acid) (in solution) &key)
  (call-next-method amount-of comp in :indicator *hydronium*))

(defmethod solve (amount-of (comp base) (in solution) &key)
  (call-next-method amount-of comp in :indicator *hydroxide*)) 



(defgeneric ph (solution))

(defmethod ph ((solution solution))
  (let ((hydronium (/ (solute *hydronium* solution)
		      (volume solution)))
	(hydroxide (/ (solute *hydroxide* solution)
		      (volume solution))))
    (cond ((plusp hydronium)
	   (- (log hydronium 10)))
	  ((plusp hydroxide)
	   (- 14 (- (log hydroxide 10))))
	  (t 7))))