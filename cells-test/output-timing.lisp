(in-package :cells)

(DEFPARAMETER *master* nil)
(DEFPARAMETER *thing1* nil)
(DEFPARAMETER *thing2* nil)

(defmodel master ()
  ((id :cell nil :initarg :id :initform (c-in nil) :accessor id)
   (onn :initarg :onn :initform (c-in nil) :accessor onn)))

(defmodel user ()
    ((id :initarg :id :initform (c-in nil) :accessor id)
     (uses :initarg uses :accessor uses
       :initform (c? (when (onn *master*)
                       (trc "uses rule collecting" self)
                       (list *thing1* *thing2*))))))

(defmodel thing ()
  ((id :cell nil :initarg :id :initform (c-in nil) :accessor id)
   (used-by :initform (c-in nil) :accessor used-by)))

(defmethod print-object ((u user) s)
  (format s (string (id u))))

(defmethod print-object ((u thing) s)
  (format s (string (id u))))

(defobserver uses ()
  (when new-value
    (with-integrity (:change `(:uses-block ,self))
      (trc "output uses" self new-value old-value)
      (let ((add (set-difference new-value old-value))
            (remove (set-difference old-value new-value)))
        (dolist (thing remove)
          (setf (used-by thing) (remove self (used-by thing))))
        (dolist (thing add)
          (push self (used-by thing)))))))

(def-cell-test tfb ()
  (cells-reset)
  (let* ((*master* (make-instance 'master))
        (u1 (make-instance 'user :id :u1))
        (u2 (make-instance 'user :id :u2))
        (*thing1* (make-instance 'thing :id :thg1))
        (*thing2* (make-instance 'thing :id :thg2)))
    (declare (ignorable u1 u2))
    (setf (onn *master*) t)
    (ct-assert (= 2 (length (uses u1))))
    (ct-assert (= 2 (length (uses u2))))
    (values)))

#+test
(tfb)

(defparameter *user-steps* nil)

(defmodel k-choice (family)
  ((chosen :initform (c? (car (^kids))) :accessor chosen))
  (:default-initargs
      :kids (c? (the-kids
                 (loop for x below 3
                       collecting (make-kid 'k-option
                                    :md-name x))))))

(defmodel k-option (model)
  ((selected-p :accessor selected-p
     :initform (c? (eq self (chosen .parent))))))

(def-cell-test output-user ()
  (cells-reset (lambda (user-q)
                (loop for user-q-item = (fifo-pop user-q)
                    while user-q-item
                    do (destructuring-bind (defer-info . task) user-q-item
                         (declare (ignorable defer-info))
                         (print `(defer-info ,defer-info))
                         (funcall task)))))
  (let (*user-steps*)
    (make-instance 'k-choice)
    (loop for s in (nreverse *user-steps*)
          do (print s))
    (values)))

#+test
(output-user)

(defmethod md-awaken :before ((self k-option))
  (push `(cre-opt,(md-name self)) *user-steps*))

(defmethod md-awaken :before ((self k-choice))
  (push `(cre-chc ,(md-name self)) *user-steps*))

(defobserver chosen ()
  (with-integrity (:client 'chosen)
    (when new-value
      (push `(chosen ,(md-name new-value)) *user-steps*))))


;; make sure unasked ruled Cells get initial change
;;

(defmodel c3-changer (model)
  ((ch :cell nil :initform nil :accessor ch :initarg :ch)
   (c3x :initform (c-in 0) :accessor c3x :initarg :c3x)
   (c3y :initform 42 :accessor c3y :initarg :c3y)
   (c3z :initform (c? (+ (^c3x) (^c3y))) :accessor c3z :initarg :c3z)))

(defobserver c3x ()
  (push :c3x (ch self)))

(defobserver c3y ()
  (push :c3y (ch self)))

(defobserver c3z ()
  (push :c3z (ch self)))

(def-cell-test c3-changer ()
  (cells-reset)
  (let ((c3 (make-instance 'family
              :kids (c? (the-kids
                         (make-kid 'c3-changer))))))
    (ct-assert (= 3 (length (ch (car (kids c3))))))))


#+test
(c3-changer)