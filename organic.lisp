(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :organic-chemistry
  (:use :common-lisp
	:data
	:core
	:acid-base) ;may be necessary for dealing with organic acids
  (:nicknames :org-chem))

(in-package :organic-chemistry)