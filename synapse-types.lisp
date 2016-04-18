;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells)

(export! f-find)

(defmacro f-find (synapse-id sought where)
  `(call-f-find ,synapse-id ,sought ,where))

(defun call-f-find (synapse-id sought where)
  (with-synapse synapse-id ()
    (bif (k (progn
              (find sought where)))
      (values k :propagate)
      (values nil :no-propagate))))

(defmacro f-sensitivity (synapse-id (sensitivity &optional subtypename) &body body)
  `(call-f-sensitivity ,synapse-id ,sensitivity ,subtypename (lambda () ,@body)))

(defun call-f-sensitivity (synapse-id sensitivity subtypename body-fn)
  (with-synapse synapse-id (prior-fire-value)
    (let ((new-value (funcall body-fn)))
      ;(trc "f-sensitivity fire-p decides new" new-value :from-prior prior-fire-value :sensi sensitivity)
      (let ((prop-code (if (or (xor prior-fire-value new-value)
                             (eko (nil "sens fire-p decides" new-value prior-fire-value sensitivity)
                                (delta-greater-or-equal
                                 (delta-abs (delta-diff new-value prior-fire-value subtypename)
                                   subtypename)
                                 (delta-abs sensitivity subtypename) 
                                 subtypename)))
                            :propagate
                          :no-propagate)))
        (values (if (eq prop-code :propagate)
                    (progn
                      (trc nil "sense prior fire value now" new-value)
                      (setf prior-fire-value new-value))
                  new-value) prop-code)))))

(defmacro f-delta (synapse-id (&key sensitivity (type 'number)) &body body)
  `(call-f-delta ,synapse-id ,sensitivity ',type (lambda () ,@body)))

(defun call-f-delta (synapse-id sensitivity type body-fn)
  (with-synapse synapse-id (last-relay-basis last-bound-p delta-cum)
       (let* ((new-basis (funcall body-fn))
              (threshold sensitivity)
              (tdelta (delta-diff new-basis
                         (if last-bound-p
                             last-relay-basis
                           (delta-identity new-basis type))
                         type)))
         (trc nil "tdelta, threshhold" tdelta threshold)
         (setf delta-cum tdelta)
         (let ((propagation-code
                (when threshold
                  (if (delta-exceeds tdelta threshold type)
                      (progn
                        (setf last-bound-p t)
                        (setf last-relay-basis new-basis)
                        :propagate)
                    :no-propagate))))
           (trc nil "f-delta returns values" delta-cum propagation-code)
           (values delta-cum propagation-code)))))

(defmacro f-plusp (key &rest body)
  `(with-synapse ,key (prior-fire-value) 
     (let ((new-basis (progn ,@body)))
       (values new-basis (if (xor prior-fire-value (plusp new-basis))
                             (progn
                               (setf prior-fire-value (plusp new-basis))
                               :propagate)
                           :no-propagate)))))

(defmacro f-zerop (key &rest body)
  `(with-synapse ,key (prior-fire-value) 
     (let ((new-basis (progn ,@body)))
       (values new-basis (if (xor prior-fire-value (zerop new-basis))
                             (progn
                               (setf prior-fire-value (zerop new-basis))
                               :propagate)
                           :no-propagate)))))



;;;(defun f-delta-list (&key (test #'true))
;;;  (with-synapse (prior-list)
;;;             :fire-p (lambda (syn new-list)
;;;                           (declare (ignorable syn))
;;;                           (or (find-if (lambda (new)
;;;                                            ;--- gaining one? ----
;;;                                            (and (not (member new prior-list))
;;;                                                 (funcall test new)))
;;;                                        new-list)
;;;                               (find-if (lambda (old)
;;;                                            ;--- losing one? ----
;;;                                            (not (member old new-list))) ;; all olds have passed test, so skip test here
;;;                                        prior-list)))
;;;             
;;;             :fire-value (lambda (syn new-list)
;;;                                (declare (ignorable syn))
;;;                                ;/// excess consing on long lists
;;;                                (setf prior-list (remove-if-not test new-list)))))

;;;(defun f-find-once (finder-fn)
;;;  (mk-synapse (bingo bingobound)
;;;
;;;             :fire-p (lambda (syn new-list)
;;;                            (declare (ignorable syn))
;;;                            (unless bingo ;; once found, yer done
;;;                              (setf bingobound t
;;;                                bingo (find-if finder-fn new-list))))
;;;
;;;             :fire-value (lambda (syn new-list)
;;;                                (declare (ignorable syn))
;;;                                (or bingo
;;;                                    (and (not bingobound) ;; don't bother if fire? already looked
;;;                                         (find-if finder-fn new-list))))))
                                
;;;(defun fdifferent ()
;;;  (mk-synapse (prior-object)
;;;    :fire-p (lambda (syn new-object)
;;;              (declare (ignorable syn))
;;;              (trc nil  "fDiff: prior,new" (not (eql new-object prior-object))
;;;                prior-object new-object)
;;;              (not (eql new-object prior-object)))
;;;    
;;;    :fire-value (lambda (syn new-object)
;;;                   (declare (ignorable syn))
;;;                   (unless (eql new-object prior-object)
;;;                     (setf prior-object new-object)))
;;;    ))


;;;(defun f-boolean (&optional (sensitivity 't))
;;;  (f-delta :sensitivity sensitivity :type 'boolean))
        

