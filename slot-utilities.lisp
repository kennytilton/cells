;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells)

(defun c-setting-debug (self slot-name c new-value)
  (declare (ignorable new-value))
  (cond
   ((null c)
    (format t "c-setting-debug > constant  ~a in ~a may not be altered..init to (c-in nil)"
      slot-name self)
        
    (c-break "setting-const-cell")
    (error "setting-const-cell"))
   ((c-inputp c))
   (t
    (let ((self (c-model c))
          (slot-name (c-slot-name c)))
      ;(trc "c-setting-debug sees" c newvalue self slot-name)
      (when (and c (not (and slot-name self)))
        ;; cv-test handles errors, so don't set *stop* (c-stop)
        (c-break "unadopted ~a for self ~a spec ~a" c self slot-name)
        (error 'c-unadopted :cell c))
      #+whocares (typecase c
        (c-dependent
         ;(trc "setting c-dependent" c newvalue)
         (format t "c-setting-debug > ruled  ~a in ~a may not be setf'ed"
           (c-slot-name c) self)
         
         (c-break "setting-ruled-cell")
         (error "setting-ruled-cell"))
        )))))

(defun c-absorb-value (c value)
  (typecase c
    (c-drifter-absolute (c-value-incf c value 0)) ;; strange but true
    (c-drifter (c-value-incf c (c-value c) value))
    (t value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(c-value-incf)))

(defmethod c-value-incf (c (envaluer c-envaluer) delta)
  (c-assert (c-model c))
  (c-value-incf c (funcall (envalue-rule envaluer) c)
                 delta))

(defmethod c-value-incf (c (base number) delta)
  (declare (ignore c))
  (if delta
    (+ base delta)
    base))


;----------------------------------------------------------------------

(defun bd-slot-value (self slot-name)
  (slot-value self slot-name))

(defun (setf bd-slot-value) (new-value self slot-name)
  (setf (slot-value self slot-name) new-value))

(defun bd-bound-slot-value (self slot-name caller-id)
  (declare (ignorable caller-id))
  (when (bd-slot-boundp self slot-name)
    (bd-slot-value self slot-name)))

(defun bd-slot-boundp (self slot-name)
  (slot-boundp self slot-name))

(defun bd-slot-makunbound (self slot-name)
  (if slot-name ;; not in def-c-variable
    (slot-makunbound self slot-name)
    (makunbound self)))

#| sample incf
(defmethod c-value-incf ((base fpoint) delta)
  (declare (ignore model))
  (if delta
    (fp-add base delta)
    base))
|#
