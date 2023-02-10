;; -*- mode: Lisp; Syntax: Common-Lisp; Package: triple-cells; -*-
;;;
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;

(in-package :3c)

(defmacro make-observer (id form)
  `(call-make-observer ,id
     '(lambda (s p new-value prior-value prior-value?)
        (declare (ignorable s p new-value prior-value prior-value?))
        ,form)))

(defun call-make-observer (id observer)
  (add-triple id !ccc:observer-id-rule (mk-upi (prin1-to-string observer)))
  (setf (3c-observer id) (eval observer))) ;; while we're at it


;;; --- 3cell observation --------------------------------------------------------

(defun cell-observe-change (cell s p new-value prior-value prior-value?)
  (cond
   (cell
    (loop for observer in (get-triples-list :s cell :p !ccc:observer-is)
        do (funcall (3c-observer (object observer)) s p
             new-value prior-value prior-value?)))
   (p (loop for observer in (get-triples-list :s p :p !ccc:observer-id-rule)
          do (funcall (3c-observer-from-rule-triple observer) s p
               new-value prior-value prior-value?)))))   

;;;(defun cell-observe-change (cell s p new-value prior-value prior-value?)
;;;  (trc "observing" p new-value)
;;;  (if (get-triple :s cell :p !ccc:observer-is) ; just need one to decide to schedule
;;;      (let ((o (new-blank-node))) ;; o = observation, an instance of a cell to be observed and its parameters
;;;        (add-triple o !ccc:obs-s cell)
;;;        (add-triple o !ccc:obs-p cell)
;;;        (add-triple o !ccc:obs-new-value (mk-upi new-value))
;;;        (add-triple o !ccc:obs-prior-value (mk-upi prior-value))
;;;        (add-triple o !ccc:obs-prior-value? (mk-upi prior-value?))
;;;        (add-triple !ccc:obs-queue (mk-upi (get-internal-real-time)) o))
;;;    (trc "unobserved" s p)))

;;;(defun process-observer-queue ()
;;;  (index-new-triples)
;;;  (let ((oq (get-triples-list :s !ccc:obs-queue)))
;;;    (loop for observation in (mapcar 'object oq)
;;;        for s = (object (get-sp observation !ccc:obs-s))
;;;        for p = (object (get-sp observation !ccc:obs-p))
;;;        for new-value = (get-sp-value observation !ccc:obs-new-value)
;;;        for prior-value = (get-sp-value observation !ccc:obs-prior-value)
;;;        for prior-value? = (get-sp-value observation !ccc:obs-prior-value)
;;;        do (loop for observer in (get-triples-list :s s :p !ccc:observer-is)
;;;               do (funcall (3c-observer (object observer)) s p
;;;                    new-value prior-value prior-value?)))))


;;; ----------------------------------------------------



(defun (setf 3c-observer) (function c-node)
  (assert (functionp function) () "3c-observer setf not rule: ~a ~a" (type-of function) function)
  (setf (gethash c-node *3c-observers*) function))

(defun 3c-observer (c-node &aux (unode (part->string c-node)))
  (or (gethash unode *3c-observers*)
    (setf (gethash unode *3c-observers*)
      (let ((fn$ (get-sp-value unode !ccc:observer-id-rule)))
        (assert fn$)
        
        (eval (read-from-string fn$))))))

(defun 3c-observer-from-rule-triple (tr)
  (let ((fn$ (triple-value tr)))
        (assert fn$)
        (eval (read-from-string fn$))))
