;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cells)

(defun md-awake (self) (eql :awake (md-state self)))

(defun fm-grandparent (md)
  (fm-parent (fm-parent md)))


(defmethod md-release (other)
  (declare (ignorable other)))

(export! mdead)
;___________________ birth / death__________________________________
  
(defgeneric mdead (self)
  (:method ((self model-object))
    (unless *not-to-be* ;; weird - let's folks function for not-to-be cleanup
      (eq :eternal-rest (md-state self))))

  (:method (self)
    (declare (ignore self))
    nil))

(defparameter *ntb-dbg* nil)

(export! *ntb-dbg*)

(defgeneric not-to-be (self)
  (:method (other)
    (declare (ignore other)))
  (:method ((self cons))
    (not-to-be (car self))
    (not-to-be (cdr self)))
  (:method ((self array))
    (loop for s across self
          do (not-to-be s)))
  (:method ((self hash-table))
    (maphash (lambda (k v)
               (declare (ignorable k))
               (not-to-be v)) self))

  (:method ((self model-object))
    (when *ntb-dbg*
      (trcx not2be self)
      (when (and *md-awake* (not (gethash self *md-awake*)))
        (trcx not2be-not-awake!!!! self)))
    (setf (md-census-count self) -1)
    (md-quiesce self)
    (md-awake-remove self))

  (:method :before ((self model-object))
    (loop for slot-name in (md-owning-slots self)
        do (not-to-be (slot-value self slot-name))))

  (:method :around ((self model-object))
    (declare (ignorable self))
    (let ((*not-to-be* t)
          (dbg nil))
      
      (flet ((gok ()
               (if (or (eq (md-state self) :eternal-rest)
                     (md-doomed self))
                   (trc nil "n2be bailing already dead or doomed" self (md-state self)(md-doomed self))
                 (progn
                   (setf (md-doomed self) t)
                   (call-next-method)
                   (setf (fm-parent self) nil
                     (md-state self) :eternal-rest)
                   (md-awake-remove self)
                   (md-map-cells self nil
                     (lambda (c)
                       (c-assert (eq :quiesced (c-state c)) ()
                         "Cell ~a of dead model ~a not quiesced. Was not-to-be shadowed by
 a primary method? Use :before instead." c self))) ;; fails if user obstructs not.to-be with primary method (use :before etc)
                   
                   ))))
        (if (not dbg)
            (gok)
          (wtrc (0 100 "not.to-be nailing" self (when (typep self 'family)
                                                  (mapcar 'type-of (slot-value self '.kids))))
            (gok)
            (when dbg (trc "finished nailing" self))))))))



(defun md-quiesce (self)
  (trc nil "md-quiesce nailing cells" self (type-of self))
  (md-map-cells self nil (lambda (c)
                           (trc nil "quiescing" c)
                           (c-assert (not (find c *call-stack*)))
                           (c-quiesce c)))
  (when (register? self)
    (fm-check-out self)))

(defun c-quiesce (c)
  (typecase c
    (cell 
     (trc nil "c-quiesce unlinking" c)
     (c-unlink-from-used c)
     (dolist (caller (c-callers c))
       (setf (c-value-state caller) :uncurrent)
       (trc nil "c-quiesce totlalaly unlinking caller and making uncurrent" .dpid :q c :caller caller)
       (c-unlink-caller c caller))
     (setf (c-state c) :quiesced) ;; 20061024 for debugging for now, might break some code tho
     )))

(defparameter *to-be-dbg* nil)

(defmacro make-kid (class &rest initargs)
  `(make-instance ,class
     ,@initargs
     :fm-parent (progn (assert self) self)))

(defvar *c-d-d*)
(defvar *max-d-d*)

(defparameter *model-pop* nil)

(export! md-census-start md-census-report md-census-count)

(defun md-census-start ()
  (setf *model-pop* (make-hash-table :test 'eq)))

(defun (setf md-census-count) (delta self)
  (when *model-pop*
    (incf (gethash (type-of self) *model-pop* 0) delta)
    #-its-alive!
    (when (minusp (gethash (type-of self) *model-pop* 0))
      (warn  "minus pop ~a" self))))

(defun md-census-report ()
  (when *model-pop*
    (loop for (ct . type)
        in (sort (let (raw)
                   (maphash (lambda (k v)
                              (push (cons v k) raw))
                     *model-pop*)
                   raw) '< :key 'car)
        unless (zerop ct)
        do (trc "pop" ct type))))

#+test
(md-census-report)

#+test
(md-census-count)

(defun md-census-count (&optional type)
  (when *model-pop*
  (if type
      (gethash type *model-pop* 0)
    (loop for v being the hash-values of *model-pop*
          summing v))))


(defun count-model (self &key count-cells &aux (ccc 0))
  
  (setf *c-d-d* (make-hash-table :test 'eq) *max-d-d* 0)
  (let ((*counted* (make-hash-table :test 'eq :size 5000)))
    (with-metrics (t nil "cells statistics for" self)
      (labels ((cc (self from)
                 (unless (gethash self *counted*)
                   (setf (gethash self *counted*) t)
                   (typecase self
                     (cons (cc (car self) from)
                       (cc (cdr self) from))
                     #+nahhhh (mathx::box (count-it! :mathx-box-struct)
                                    (cc (mathx::bx-mx self) from))
                     (model
                      (when (zerop (mod (incf ccc) 100))
                        (trc "cc" (md-name self) (type-of self)))
                      (count-it! :thing)
                      (count-it! :thing (type-of self))
                      #+nahhhh (when (typep self 'mathx::problem)
                                (count-it! :thing-from (type-of self) (type-of from)))
                      (when count-cells
                        (loop for (nil . c) in (cells self)
                            do (count-it! :live-cell)
                              ;(count-it! :live-cell id)
                              (when (c-lazy c)
                                (count-it! :lazy)
                                (count-it! :lazy (c-value-state c)))
                              (typecase c
                                (c-dependent
                                 (count-it! :dependent-cell)
                                 #+chill (loop repeat (length (c-useds c))
                                             do (count-it! :cell-useds)
                                               (count-it! :dep-depth (c-depend-depth c))))
                                (otherwise (if (c-inputp c)
                                               (progn
                                                 (count-it! :c-input-altogether)
                                                 ;(count-it! :c-input id)
                                                 )
                                             (count-it! :c-unknown))))
                              
                              (loop repeat (length (c-callers c))
                                  do (count-it! :cell-callers)))
                        
                        (loop repeat (length (cells-flushed self))
                            do (count-it! :flushed-cell #+toomuchinfo id)))
                      
                      (loop for slot in (md-owning-slots self) do
                            (loop for k in (let ((sv (slot-value self slot)))
                                             (if (listp sv) sv (list sv)))
                                do (cc k self)))
                      #+nahhh
                      (progn
                        (when (typep self 'mathx::mx-optr)
                          (cc (mathx::opnds self) from))
                        (when (typep self 'mathx::math-expression)
                          (count-it! :math-expression))))
                     (otherwise
                      (count-it (type-of self)))))))
        (cc self nil)))))

(defun c-depend-depth (ctop)
  (if (null (c-useds ctop))
      0
    (or (gethash ctop *c-d-d*)
      (labels ((cdd (c &optional (depth 1) chain)
                 (when (and (not (c-useds c))
                         (> depth *max-d-d*))
                   (setf *max-d-d* depth)
                   (trc "new dd champ from user"  depth :down-to c)
                   (when (= depth 41)
                     (trc "end at" (c-slot-name c) :of (type-of (c-model c)))
                     (loop for c in chain do
                           (trc "called by" (c-slot-name c) :of (type-of (c-model c))))))
                 (setf (gethash c *c-d-d*)
                   ;(break "c-depend-depth ~a" c)
                   (progn
                     ;(trc "dd" c)
                     (1+ (loop for u in (c-useds c)
                             maximizing (cdd u (1+ depth) (cons c chain))))))))
        (cdd ctop)))))
    