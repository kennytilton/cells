(in-package :cells)

(defvar *tests* ())


(defmacro deftest (name form &rest values)
  "Po man's RT."
  (let ((test-name (intern (format nil "TEST ~A" name))))
    `(progn
       (defun ,test-name ()
	 (let ((name ',name)
	       (form ',form)
	       (expected-values ',values)
	       (actual-values (multiple-value-list
			       (handler-case ,form
				 (error (val) val)))))
	   (assert (equal actual-values ',values) (actual-values)
		   "Test ~S failed~% ~
                    Form: ~A~% ~
                    Expected values: ~{~S~^; ~}~% ~
                    Actual values: ~{~S~^; ~}"
		   name form expected-values actual-values)
	   ',name))
       (pushnew ',name *tests*)
       ',name)))

(defun do-test (name)
  (let ((test (intern (format nil "TEST ~A" name) (symbol-package name))))
    (funcall test)))

(defun cv-test-lazy ()
  (every #'do-test (reverse *tests*)))

(defmacro unbound-error-p (form)
  `(handler-case
       (progn
         ;;(print `(checking unbound error ,',form))
         ,form nil)
     (unbound-cell () t)))

(defun make-cell-valid (self slot)
  (setf (c-state (md-slot-cell self slot)) :valid))

(defmodel unbound-values ()
  ((val1 :initform (c-input ()) :initarg val1 :accessor test-val1)
   (val2 :initform (c-input ()) :initarg val2 :accessor test-val2)))

(defmodel unbound-formulas (unbound-values)
  ((formula :initform nil ;; no longer an exception made for unechoed slots re c-awakening
     :accessor test-formula)
   (lazy-formula :initform (c-formula (:lazy t)
                             (^test-val1)
                             (^test-val2))
     :accessor test-lazy-formula)))

(defmodel unbound-formulas2 (unbound-values)
  ((formula :initform (c? (^test-val1)
                        (^test-val2))
     :accessor test-formula)
   (lazy-formula :initform (c-formula (:lazy t)
                             (^test-val1)
                             (^test-val2))
     :accessor test-lazy-formula)))

(deftest unbound-values
    (let ((self (make-instance 'unbound-values)))
      (values (unbound-error-p (test-val1 self))
	      (unbound-error-p (test-val2 self))))
  t t)

(deftest md-slot-makunbound
    (let ((self (progn (make-instance 'unbound-values
			 'val1 (c-in nil) 'val2 (c-in nil)))))
      (md-slot-makunbound self 'val1)
      (md-slot-makunbound self 'val2)
      (values (unbound-error-p (test-val1 self))
	      (unbound-error-p (test-val2 self))))
  t t)

(deftest formula-depends-on-unbound
    (let ((obj1 (progn (make-instance 'unbound-formulas)))
	  (obj2 (progn (make-instance 'unbound-formulas))))
      (values ;(unbound-error-p (test-formula obj1))
	      (unbound-error-p (test-lazy-formula obj1))

	      (unbound-error-p (test-lazy-formula obj2))
	      ;(unbound-error-p (test-formula obj2))
       ))
  t t)

(deftest unbound-ok-for-unbound-formulas
    (unbound-error-p
     (progn (let ((self (progn (make-instance 'unbound-formulas))))
	      (setf (test-val1 self) t
		    (test-val2 self) t))
	    (let ((self (progn (make-instance 'unbound-formulas))))
	      (setf (test-val2 self) t
		    (test-val1 self) t))))
  nil)

(deftest unbound-errs-for-eager
    (let ((self (progn (make-instance 'unbound-formulas2
			 'val1 (c-in 1) 'val2 (c-in 2)))))
      (values (test-formula self)
	     (unbound-error-p (md-slot-makunbound self 'val1))
	     (unbound-error-p (md-slot-makunbound self 'val2))
        ))
  2 t t
  )

(deftest unbound-ok-for-unchecked-lazy
    (let ((self (progn (make-instance 'unbound-formulas
			 'val1 (c-in 1) 'val2 (c-in 2)))))
      (values (test-lazy-formula self)
	      (unbound-error-p (md-slot-makunbound self 'val1))
	      (unbound-error-p (md-slot-makunbound self 'val2))))
  2 nil nil)

#+(or) 
(cv-test-lazy)

(defparameter *lz1-count* 0)

(defmd lz-simple ()
  (lz1 (c?_ (incf *lz1-count*)
          (* 2 (^lz2))))
   (lz2 (c-in 0)))

(defun lz-test ()
  (cells-reset)
  (let ((*lz1-count* 0)
        (lz (make-instance 'lz-simple)))
    (assert (zerop *lz1-count*))
    (incf (lz2 lz))
    (assert (zerop *lz1-count*))
    (assert (= (lz1 lz) 2))
    (assert (= 1 *lz1-count*))
    lz))

#+test
(lz-test)
