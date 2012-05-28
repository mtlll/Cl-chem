(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :core
  (:use :common-lisp
	:data)
  (:export compound
	   elements
	   charge
	   molar-mass
	   chemistry-thing
	   solution
	   solutes
	   volume
	   with-solutes
	   solve
	   compound=))

(in-package :core)

(defclass chemistry-thing () ())

(defclass compound (chemistry-thing)
  ((elements            ; List of two part tuples(atom name and number of atoms).
    :reader elements
    :initarg :elements)
   (charge
    :accessor charge
    :initarg :charge
    :initform 0)
   (molar-mass
    :accessor molar-mass)))

(defgeneric compound-p (thing))

(defmethod compound-p ((thing compound))
  t)

(defmethod compound-p (thing)
  nil)

(defun mass-of-element (element)
  (let ((thing (first element))) 
    (if (compound-p thing)
	(* (molar-mass thing)
	   (second element))
	(* (cdr (assoc (first element) *periodic-table* :test #'string=))
	   (second element)))))

(defun charge-of-element (element)
  (let ((thing (first element)))
    (cond ((compound-p thing)
	   (* (charge thing)
	      (second element))) 
	  ((= (length element) 2)
	   0)
	  (t (* (third element)
		(second element))))))

(defmethod initialize-instance :after ((compound compound) &key)
  (with-slots (elements molar-mass charge) compound
    (setf molar-mass
	  (reduce #'+ elements :key #'mass-of-element))
    (unless charge
      (setf charge (reduce #'+ elements :key #'charge-of-element :initial-value charge)))))

(defgeneric compound= (com1 com2))

(defmethod compound= ((com1 compound) (com2 compound))
  (and (= (charge com1) (charge com2))
       (equalp (elements com1)
	       (elements com2))))

(defclass solution (chemistry-thing)
  ((solutes
    :accessor solutes
    :initform (make-hash-table :test #'equal))
   (volume
    :accessor volume
    :initarg volume
    :initform 1)))

;; No one likes messing around with gethash all over the place.
(defun solute (solution compound)
  (gethash (chemistry-thing-to-string compound)
	   (solutes solution)
	   0))

(defun (setf solute) (new-val solution compound)
  (setf (gethash (chemistry-thing-to-string compound)
	   (solutes solution)
	   0)
	new-val))

;; Look at me and my fanciness!
(defmacro with-solutes (slots solution &body body)
  (let ((sol (gensym)))
    `(let ((,sol ,solution))
       (with-slots (solutes) ,sol
	 (symbol-macrolet ,(loop for (name slot) in slots collecting `(,name (solute ,slot ,sol)))
	   ,@body)))))

(defgeneric solve (amount-of comp in &key))

(defmethod solve (amount-of (comp compound) (in solution) &key)
  (incf (solute in comp) amount-of))

(defun lookup (thing alist)
  (cdr (assoc thing alist)))

(defun digits (num)
  (let ((result nil))
    (do ((i 0)
	 (j num))
	((zerop j)
	 (push i result))
      (cond ((zerop (rem j 10))
	     (push i result)
	     (setf j (/ j 10))
	     (setf i 0)) 
	    (t
	     (incf i)
	     (decf j))))
    result))

(defun num-to-x-script (num &optional (x *superscript-digits*))
  (let ((digits (digits num)))
    (with-output-to-string (s)
      (loop for dig in digits
	 do (princ (lookup dig x) s)))))

(defun charge-to-superscript (charge)
  (with-output-to-string (s)
    (when (/= charge 0)
      (when (or (> charge 1)
		(< charge -1))
	(princ (num-to-x-script (abs charge)) s))
      (princ (lookup (/ charge (abs charge))
		     *superscript-signs*)
	     s))))


(defgeneric element-to-string (element index))

(defmethod element-to-string ((element compound) index)
  (with-accessors ((elements elements)) element
    (if (and (> (length elements) 1)
	     (> index 1))
	(element-to-string
	 (format nil "(~a)" (chemistry-thing-to-string element :print-charge-p nil))
	 index)
	(chemistry-thing-to-string element :print-charge-p nil))))

(defmethod element-to-string (element index)
  (with-output-to-string (s)
    (princ element s)
    (when (> index 1)
      (princ (num-to-x-script index *subscript-digits*) s))))

(defgeneric chemistry-thing-to-string (thing &key))

(defmethod chemistry-thing-to-string ((thing chemistry-thing) &key)
  "")
(defmethod chemistry-thing-to-string ((thing compound) &key (print-charge-p t))
  (with-accessors ((elements elements)
		   (charge charge))
      thing
    (with-output-to-string (s)
      (loop for (element index) in elements
	   do (princ (element-to-string element index) s))
      (when (and print-charge-p
		 (/= charge 0))
	(princ (charge-to-superscript charge) s)))))

(defmethod print-object ((object chemistry-thing) stream)
  (format stream "#<~s ~s>"
	  (type-of object)
	  (chemistry-thing-to-string object)))
