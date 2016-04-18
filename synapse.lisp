;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mk-synapse f-delta f-sensitivity f-plusp f-zerop fdifferent with-synapse)))

(defmacro with-synapse (synapse-id (&rest closure-vars) &body body)
  (let ((syn-id (gensym)))
    `(let* ((,syn-id ,synapse-id)
            (synapse (or (find ,syn-id (cd-useds *depender*) :key 'c-slot-name)
                       (let ((new-syn
                              (let (,@closure-vars)
                                (make-c-dependent
                                 :model (c-model *depender*)
                                 :slot-name ,syn-id
                                 :code #+live nil #-live ',body
                                 :synaptic t
                                 :rule (c-lambda ,@body)))))
                         (record-caller new-syn)
                         new-syn))))
       (prog1
           (multiple-value-bind (v p)
               (with-integrity ()
                 (ensure-value-is-current synapse :synapse *depender*))
             (values v p))
         (record-caller synapse)))))


;__________________________________________________________________________________
;

(defmethod delta-exceeds (bool-delta sensitivity (subtypename (eql 'boolean)))
  (unless (eql bool-delta :unchanged)
    (or (eq sensitivity t)
        (eq sensitivity bool-delta))))

(defmethod delta-diff ((new number) (old number) subtypename)
  (declare (ignore subtypename))
  (- new old))

(defmethod delta-identity ((dispatcher number) subtypename)
  (declare (ignore subtypename))
  0)

(defmethod delta-abs ((n number) subtypename)
  (declare (ignore subtypename))
  (abs n))

(defmethod delta-exceeds ((d1 number) (d2 number) subtypename)
  (declare (ignore subtypename))
  (> d1 d2))

(defmethod delta-greater-or-equal ((d1 number) (d2 number) subtypename)
  (declare (ignore subtypename))
  (>= d1 d2))

;_________________________________________________________________________________
;
(defmethod delta-diff (new old (subtypename (eql 'boolean)))
   (if new
       (if old
           :unchanged
         :on)
     (if old
         :off
       :unchanged)))


(defmethod delta-identity (dispatcher (subtypename (eql 'boolean)))
   (declare (ignore dispatcher))
   :unchanged)

