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

(defstruct (cell (:conc-name c-))
  model
  slot-name
  value
  
  inputp ;; t for old c-variable class
  synaptic
  (caller-store (make-fifo-queue) :type cons) ;; (C3) probably better to notify callers FIFO
  
  (state :nascent :type symbol) ;; :nascent, :awake, :optimized-away
  (value-state :unbound :type symbol) ;; {:unbound | :unevaluated | :uncurrent | :valid}
                                                       ; uncurrent (aka dirty) new for 06-10-15. we need this so
                                                       ; c-quiesce can force a caller to update when asked
                                                       ; in case the owner of the quiesced cell goes out of existence
                                                       ; in a way the caller will not see via any kids dependency. Saw
                                                       ; this one coming a long time ago: depending on cell X implies
                                                       ; a dependency on the existence of instance owning X
  (pulse 0 :type fixnum)
  (pulse-last-changed 0 :type fixnum) ;; lazys can miss changes by missing change of X followed by unchange of X in subsequent DP
  (pulse-observed 0 :type fixnum)
  lazy
  (optimize t)
  debug
  md-info)



;_____________________ print __________________________________

#+sigh
(defmethod print-object :before ((c cell) stream)
  (declare (ignorable stream))
  #+shhh (unless (or *stop* *print-readably*)
    (format stream "[~a~a:" (if (c-inputp c) "i" "?")
      (cond
       ((null (c-model c)) #\0)
       ((eq :eternal-rest (md-state (c-model c))) #\_)
       ((not (c-currentp c)) #\#)
       (t #\space)))))

(defmethod print-object ((c cell) stream)
  (declare (ignorable stream))
  (if *stop*
      (format stream "<~d:~a ~a/~a = ~a>"
        (c-pulse c)
        (subseq (string (c-state c)) 0 1)
        (symbol-name (or (c-slot-name c) :anoncell))
        (md-name (c-model c))
        (type-of (c-value c)))
    (let ((*print-circle* t))
      #+failsafe (format stream "~a/~a" (c-model c)(c-slot-name c))
      (if *print-readably*
          (call-next-method)
        (progn
          (c-print-value c stream)
          (format stream "<~d:~a ~a/~a = ~a>"
            (c-pulse c)
            (subseq (string (c-state c)) 0 1)
            (symbol-name (or (c-slot-name c) :anoncell))
            (print-cell-model (c-model c))
            (if (consp (c-value c))
                "LST" (c-value c))))))))

(export! print-cell-model)

(defgeneric print-cell-model (md)
  (:method (other) (print-object other nil)))

(defmethod trcp :around ((c cell))
  (and ;*c-debug*
    (or (c-debug c)
      (call-next-method))))

(defun c-callers (c)
  "Make it easier to change implementation"
  (fifo-data (c-caller-store c)))

(defun caller-ensure (used new-caller)
  (unless (find new-caller (c-callers used))
    (trc nil "caller-ensure fifo-adding new-caller" new-caller :used used)
    (fifo-add (c-caller-store used) new-caller)))

(defun caller-drop (used caller)
  (fifo-delete (c-caller-store used) caller))

; --- ephemerality --------------------------------------------------
; 
; Not a type, but an option to the :cell parameter of defmodel
;
(defun ephemeral-p (c)
  (eql :ephemeral (md-slot-cell-type (type-of (c-model c)) (c-slot-name c))))

(defun ephemeral-reset (c)
  (when (ephemeral-p c) ;; so caller does not need to worry about this
    ;
    ; as of Cells3 we defer resetting ephemerals because everything
    ; else gets deferred and we cannot /really/ reset it until
    ; within finish_business we are sure all callers have been recalculated
    ; and all outputs completed.
    ;
    ; ;; good q: what does (setf <ephem> 'x) return? historically nil, but...?
    ;
    ;;(trcx bingo-ephem c)
    (with-integrity (:ephemeral-reset c)
      (trc nil "!!!!!!!!!!!!!! ephemeral-reset resetting:" c)
      (md-slot-value-store (c-model c) (c-slot-name c) nil)
      (setf (c-value c) nil))))

; -----------------------------------------------------

(defun c-validate (self c)
  (when (not (and (c-slot-name c) (c-model c)))
    (format t "~&unadopted cell: ~s md:~s" c self)
    (c-break "unadopted cell ~a ~a" self c)
    (error 'c-unadopted :cell c)))

(defstruct (c-ruled
            (:include cell)
            (:conc-name cr-))
  (code nil :type list) ;; /// feature this out on production build
  rule)

(defun c-optimized-away-p (c)
  (eq :optimized-away (c-state c)))

;----------------------------

(defmethod trcp-slot (self slot-name)
  (declare (ignore self slot-name)))

(defstruct (c-dependent
            (:include c-ruled)
            (:conc-name cd-))
  ;; chop (synapses nil :type list)
  (useds nil :type list)
  (usage (blank-usage-mask)))

(defun blank-usage-mask ()
  (make-array 16 :element-type 'bit
    :initial-element 0))

(defstruct (c-drifter
            (:include c-dependent)))

(defstruct (c-drifter-absolute
            (:include c-drifter)))

;_____________________ accessors __________________________________

(defmethod c-useds (other) (declare (ignore other)))
(defmethod c-useds ((c c-dependent)) (cd-useds c))

(defun c-validp (c)
  (eql (c-value-state c) :valid))

(defun c-unboundp (c)
  (eql :unbound (c-value-state c)))


;__________________

(defmethod c-print-value ((c c-ruled) stream)
  (format stream "~a" (cond ((c-validp c) (cons (c-value c) "<vld>"))
                            ((c-unboundp c) "<unb>")
                            ((not (c-currentp c)) "dirty")
                            (t "<err>"))))

(defmethod c-print-value (c stream)
  (declare (ignore c stream)))

