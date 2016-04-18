;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(family-values family-values-sorted
            sort-index sort-direction sort-predicate sort-key
            ^sort-index ^sort-direction ^sort-predicate ^sort-key)))

(defmodel family-values (family)
  (
   (kv-collector :initarg :kv-collector
     :initform #'identity
     :reader kv-collector)
   
   (kid-values :initform (c? (when (kv-collector self)
                               (funcall (kv-collector self) (^value))))
     :accessor kid-values
     :initarg :kid-values)
   
   (kv-key :initform #'identity
     :initarg :kv-key
     :reader kv-key)
   
   (kv-key-test :initform #'equal
     :initarg :kv-key-test
     :reader kv-key-test)
   
   (kid-factory :initform #'identity
     :initarg :kid-factory
     :reader kid-factory)
   
   (.kids :initform (c? (c-assert (listp (kid-values self)))
                      (let ((new-kids (mapcan (lambda (kid-value)
                                                (list (or (find kid-value .cache
                                                            :key (kv-key self)
                                                            :test (kv-key-test self))
                                                        (trc nil "family-values forced to make new kid" 
                                                          self .cache kid-value)
                                                        (funcall (kid-factory self) self kid-value))))
                                        (^kid-values))))
                        (nconc (mapcan (lambda (old-kid)
                                         (unless (find old-kid new-kids)
                                           (when (fv-kid-keep self old-kid)
                                             (list old-kid))))
                                 .cache)
                          new-kids)))
     :accessor kids
     :initarg :kids)))

(defmethod fv-kid-keep (family old-kid)
  (declare (ignorable family old-kid))
  nil)

(defmodel family-values-sorted (family-values)
  ((sorted-kids :initarg :sorted-kids :accessor sorted-kids
     :initform nil)
   (sort-map :initform (c-in nil) :initarg :sort-map :accessor sort-map)
   (.kids :initform (c? (c-assert (listp (kid-values self)))
                 (mapsort (^sort-map)
                   (the-kids
                    (mapcar (lambda (kid-value)
                              (trc "making kid" kid-value)
                              (or (find kid-value .cache :key (kv-key self) :test (kv-key-test self))
                                (trc nil "family-values forced to make new kid" self .cache kid-value)
                                (funcall (kid-factory self) self kid-value)))
                      (^kid-values)))))
     :accessor kids
     :initarg :kids)))

(defun mapsort (map data)
  ;;(trc "mapsort map" map)
  (if map
      (stable-sort data #'< :key (lambda (datum) (or (position datum map)
                                                       ;(trc "mapsort datum not in map" datum)
                                                       (1+ (length data)))))
    data))

(defobserver sorted-kids ()
  (setf (sort-map self) new-value)) ;; cellular trick to avoid cyclicity