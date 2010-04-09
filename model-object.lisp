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

;;; --- model-object ----------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(md-name fm-parent .parent )))

(defclass model-object ()
  ((.md-state :initform :nascent :accessor md-state) ; [nil | :nascent | :alive | :eternal-rest]
   (.doomed :initform nil :accessor md-doomed) ; goes t at start of not-to-be (prolly could fold into state w/ work)
   (.fnz :initform nil  )
   (.awaken-on-init-p :initform nil :initarg :awaken-on-init-p :accessor awaken-on-init-p)
   (.cells :initform nil :accessor cells)
   (.cells-flushed :initform nil :accessor cells-flushed
                   :documentation "cells supplied but un-whenned or optimized-away")
   (adopt-ct :initform 0 :accessor adopt-ct)))

(defmethod md-finalize ((self model-object))
  (print `(:wow-fnz-non-mod ,(type-of self))))

(defmethod register? ((self model-object)))

(defmethod md-state ((self symbol))
  :alive)
;;; --- md obj initialization ------------------

(defmethod shared-initialize :after ((self model-object) slotnames
                                      &rest initargs &key fm-parent)
  (declare (ignorable initargs slotnames fm-parent))
  ;(excl:schedule-finalization self 'md-finalize)
  (setf (md-census-count self) 1) ;; bad idea if we get into reinitializing
  (md-awake-record self)
  ;
  ; for convenience and transparency of mechanism we allow client code 
  ; to intialize a slot to a cell, but we want the slot to hold the functional
  ; value, partly for ease of inspection, partly for performance, mostly
  ; because sometimes we are a slave to other libraries, such as a persistence
  ; library that does interesting things automatically based on the slot value.
  ;
  ; here we shuttle cells out of the slots and into a per-instance dictionary of cells,
  ; as well as tell the cells what slot and instance they are mediating.
  ;
  
  (when (slot-boundp self '.md-state)
    (loop for esd in (class-slots (class-of self))
        for sn = (slot-definition-name esd)
        for sv = (when (slot-boundp self sn)
                   (slot-value self sn))
        ;; do (print (list (type-of self) sn sv (typep sv 'cell)))
        when (typep sv 'cell)
        do (if (md-slot-cell-type (type-of self) sn)
               (md-install-cell self sn sv)
             (when *c-debug*
               (break "warning: cell ~a offered for non-cellular model/slot ~a/~a" sv sn (type-of self)))))
    ;
    ; queue up for awakening
    ;
    (if (awaken-on-init-p self)
        (md-awaken self)
      (with-integrity (:awaken self)
        (md-awaken self)))
    ))

(defun md-install-cell (self slot-name c &aux (c-isa-cell (typep c 'cell)))
  ;
  ; iff cell, init and move into dictionary
  ;
  (when c-isa-cell
    (count-it :md-install-cell)
    (setf
     (c-model c) self
     (c-slot-name c) slot-name
     (md-slot-cell self slot-name) c))
  ;
  ; now have the slot really be the slot
  ;
  (if c-isa-cell
      (if (c-unboundp c)
          (bd-slot-makunbound self slot-name)
        (if self
            (setf (slot-value self slot-name)
              (when (c-inputp c) (c-value c)))
          (setf (symbol-value slot-name)
            (when (c-inputp c) (c-value c)))))
    ;; note that in this else branch  "c" is a misnomer since
    ;; the value is not actually a cell
    (if self
        (setf (slot-value self slot-name) c)
      (setf (symbol-value slot-name) c))))
  
  
;;; --- awaken --------
;
; -- do initial evaluation of all ruled slots
; -- call observers of all slots

(defmethod md-awaken :around ((self model-object))
  (when (eql :nascent (md-state self))	
    
    ;(trc "awake" (type-of self))
    ;;#-its-alive!
    (call-next-method))
  self)

#+test
(md-slot-cell-type 'cgtk::label 'cgtk::container)

(defmethod md-awaken ((self model-object))
  ;
  ; --- debug stuff
  ;
  (when *stop*
    (princ #\.)
    (return-from md-awaken))
  (trc nil "md-awaken entry" self (md-state self))
  (c-assert (eql :nascent (md-state self)))
  (count-it :md-awaken)
  ;(count-it 'mdawaken (type-of self))
  
  ; ---

  (setf (md-state self) :awakening)
  
  (dolist (esd (class-slots (class-of self)))
    (bwhen (sct (md-slot-cell-type (type-of self) (slot-definition-name esd)))
      (let* ((slot-name (slot-definition-name esd))
             (c (md-slot-cell self slot-name)))
        (when *c-debug*
          (bwhen (sv (and (slot-boundp self slot-name)
                       (slot-value self slot-name)))
            (when (typep sv 'cell)
              (c-break "md-awaken ~a found cell ~a in slot ~a" self sv esd))))
        
        (cond
         ((not c)
          ;; all slots must hit any change handlers as instances come into existence to get
          ;; models fully connected to the outside world they are controlling. that
          ;; happens in awaken-cell for slots in fact mediated by cells, but as an
          ;; optimization we allow raw literal values to be specified for a slot, in
          ;; which case heroic measures are needed to get the slot to the change handler
          ;;
          ;; next is an indirect and brittle way to determine that a slot has already been output,
          ;; but I think anything better creates a run-time hit.
          ;;
          ;; until 2007-10 (unless (cdr (assoc slot-name (cells-flushed self))) ;; make sure not flushed
          ;; but first I worried about it being slow keeping the flushed list /and/ searching, then
          ;; I wondered why a flushed cell should not be observed, constant cells are. So Just Observe It
          
          (let ((flushed (md-slot-cell-flushed self slot-name)))
            (when (or (null flushed) ;; constant, ie, never any cell provided for this slot
                    (> *data-pulse-id* (c-pulse-observed flushed))) ;; unfrickinlikely
              (when flushed
                (setf (c-pulse-observed flushed) *data-pulse-id*)) ;; probably unnecessary
              (slot-value-observe slot-name self (bd-slot-value self slot-name) nil nil flushed))))

         ((find (c-lazy c) '(:until-asked :always t))
          (trc nil "md-awaken deferring c-awaken since lazy" 
            self esd))

         ((eq :nascent (c-state c))
          (c-assert (c-model c) () "c-awaken sees uninstalled cell" c)
          (c-assert (eq :nascent (c-state c)))
          (trc nil "c-awaken > awakening" c)
          (count-it :c-awaken)
                
          (setf (c-state c) :awake)
          (awaken-cell c))))))
  
  (setf (md-state self) :awake)
  self)
  
;;; --- utilities, accessors, etc --------------------------------------

(defmethod c-slot-value ((self model-object) slot)
  (slot-value self slot))

(defmethod md-slot-cell (self slot-name)
  (if self
      (cdr (assoc slot-name (cells self)))
    (get slot-name 'cell)))

(defmethod md-slot-cell-flushed (self slot-name)
  (if self
      (cdr (assoc slot-name (cells-flushed self)))
    (get slot-name 'cell)))

#+test
(get 'cgtk::label :cell-types)

(defun md-slot-cell-type (class-name slot-name)
  (assert class-name)
  (if (eq class-name 'null)
      (get slot-name :cell-type)
    (bif (entry (assoc slot-name (get class-name :cell-types)))
      (cdr entry)
      (dolist (super (class-precedence-list (find-class class-name))
                (setf (md-slot-cell-type class-name slot-name) nil))
        (bwhen (entry (assoc slot-name (get (c-class-name super) :cell-types)))
          (return-from md-slot-cell-type
            (setf (md-slot-cell-type class-name slot-name) (cdr entry))))))))

(defun (setf md-slot-cell-type) (new-type class-name slot-name)
  (assert class-name)
  (if (eq class-name 'null) ;; not def-c-variable
      (setf (get slot-name :cell-type) new-type)
    (let ((entry (assoc slot-name (get class-name :cell-types))))
      (if entry
          (prog1
            (setf (cdr entry) new-type)
            (loop for c in (class-direct-subclasses (find-class class-name))
                do (setf (md-slot-cell-type (class-name c) slot-name) new-type)))
        (cdar (push (cons slot-name new-type) (get class-name :cell-types)))))))

#+test
(md-slot-owning? 'm-index '.value)

(defun md-slot-owning? (class-name slot-name)
  (assert class-name)
  (if (eq class-name 'null)
      (get slot-name :owning) ;; might be wrong -- support for specials is unfinished w.i.p.
    (bif (entry (assoc slot-name (get class-name :direct-ownings)))
      (cdr entry)
      (bif (entry (assoc slot-name (get class-name :indirect-ownings)))
        (cdr entry)
        (cdar
         (push (cons slot-name
                 (cdr (loop for super in (cdr (class-precedence-list (find-class class-name)))
                          thereis (assoc slot-name (get (c-class-name super) :direct-ownings)))))
           (get class-name :indirect-ownings)))))))

(defun (setf md-slot-owning-direct?) (value class-name slot-name)
  (assert class-name)
  (if (eq class-name 'null) ;; global variables
      (setf (get slot-name :owning) value)
    (progn
      (bif (entry (assoc slot-name (get class-name :direct-ownings)))
        (setf (cdr entry) value)
        (push (cons slot-name value) (get class-name :direct-ownings)))
      ; -- propagate to derivatives ...
      (labels ((clear-subclass-ownings (c)
                 (loop for sub-c in (class-direct-subclasses c)
                     for sub-c-name = (c-class-name sub-c)
                     do (setf (get sub-c-name :indirect-ownings)
                          (delete slot-name (get sub-c-name :indirect-ownings) :key 'car)) ;; forces redecide
                       (setf (get sub-c-name :model-ownings) nil) ;; too much forcing full recalc like this?
                       (clear-subclass-ownings sub-c))))
        (clear-subclass-ownings (find-class class-name))))))

(defun md-owning-slots (self &aux (st (type-of self)))
  (or (get st :model-ownings)
    (setf (get st :model-ownings)
      (loop for s in (class-slots (class-of self))
          for sn = (slot-definition-name s)
          when (and (md-slot-cell-type st sn)
                 (md-slot-owning? st sn))
          collect sn))))

#+test
(md-slot-owning? 'cells::family '.kids)

(defun md-slot-value-store (self slot-name new-value)
  (trc nil "md-slot-value-store" self slot-name new-value)
  (if self
    (setf (slot-value self slot-name) new-value)
    (setf (symbol-value slot-name) new-value)))

;----------------- navigation: slot <> initarg <> esd <> cell -----------------

#+cmu
(defmethod c-class-name ((class pcl::standard-class))
  (pcl::class-name class))

(defmethod c-class-name (other) (declare (ignore other)) nil)

;; why not #-cmu?
(defmethod c-class-name ((class standard-class))
  (class-name class))

(defmethod cell-when (other) (declare (ignorable other)) nil)

(defun (setf md-slot-cell) (new-cell self slot-name)
  (if self ;; not on def-c-variables
      (bif (entry (assoc slot-name (cells self)))
        ; this next branch guessed it would only occur during kid-slotting,
        ; before any dependency-ing could have happened, but a math-editor
        ; is silently switching between implied-multiplication and mixed numbers
        ; while they type and it 
        (progn
          (trc nil "second cell same slot:" slot-name :old entry :new new-cell)
          (let ((old (cdr entry))) ;; s/b being supplanted by kid-slotter
            (declare (ignorable old))
            (c-assert (null (c-callers old)))
            (when (typep entry 'c-dependent)
              (c-assert (null (cd-useds old))))
            (trc nil "replacing in model .cells" old new-cell self)
            (rplacd entry new-cell)))
        (progn
          (trc nil "adding to model .cells" new-cell self)
          (push (cons slot-name new-cell)
            (cells self))))
    (setf (get slot-name 'cell) new-cell)))

(defun md-map-cells (self type celldo)
  (map type (lambda (cell-entry)
                (bwhen (cell (cdr cell-entry))
                       (unless (listp cell)
                         (funcall celldo cell))))
        (cells self)))
