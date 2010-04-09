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

(defparameter *ide-app-hard-to-kill* t)

(defun md-slot-value (self slot-name &aux (c (md-slot-cell self slot-name)))
  (when (mdead self)
    ;; we  used to go kersplat in here but a problem got created just to
    ;; get its solution for a hint after which the whole shebang could
    ;; be not-to-be'd but still be interrogated for the hinting purposes
    ;; so... why not? The dead can still be useful.
    ;; If the messages get annoying do something explicit to say
    ;; not-to-be-but-expecting-use
    #-its-alive!
    (trcx md-slot-value-asked slot-name self)
    (return-from md-slot-value (slot-value self slot-name)))

  (tagbody
    retry
    (when *stop*
      (if *ide-app-hard-to-kill*
          (progn
            (princ #\.)
            (return-from md-slot-value))
        (restart-case
            (error "Cells is stopped due to a prior error.")
          (continue ()
            :report "Return a slot value of nil."
            (return-from md-slot-value nil))
          (reset-cells ()
            :report "Reset cells and retry getting the slot value."
            (cells-reset)
            (go retry))))))
  
  ;; (count-it :md-slot-value slot-name)
  (if c
      (cell-read c)
    (values (slot-value self slot-name) nil)))

(defun cell-read (c)
  (assert (typep c 'cell))
  (prog1
      (with-integrity ()
        (ensure-value-is-current c :c-read nil))
    (when *depender*
      (record-caller c))))
  
(defun chk (s &optional (key 'anon))
  (when (mdead s)
    (break "model ~a is dead at ~a" s key)))

(defvar *trc-ensure* nil)

(defun qci (c)
  (when c
    (cons (md-name (c-model c)) (c-slot-name c))))


(defun ensure-value-is-current (c debug-id ensurer)
  ;
  ; ensurer can be used cell propagating to callers, or an existing caller who wants to make sure
  ; dependencies are up-to-date before deciding if it itself is up-to-date
  ;
  (declare (ignorable debug-id ensurer))
  (count-it! :ensure.value-is-current)
  ;(trc "evic entry" (qci c))
  (wtrcx (:on? nil) ("evic>" (qci c) debug-id (qci ensurer))
    ;(count-it! :ensure.value-is-current )
    #+chill 
    (when ensurer ; (trcp c)
      (count-it! :ensure.value-is-current (c-slot-name c) (md-name (c-model c))(c-slot-name ensurer) (md-name (c-model ensurer))))
    #+chill
    (when (and *c-debug* (trcp c)
            (> *data-pulse-id* 650))
      (bgo ens-high))
    
    (trc nil ; c ;; (and *c-debug* (> *data-pulse-id* 495)(trcp c))
      "ensure.value-is-current > entry1" debug-id (qci c) :st (c-state c) :vst (c-value-state c)
      :my/the-pulse (c-pulse c) *data-pulse-id* 
      :current (c-currentp c) :valid (c-validp c))
    
    #+nahhh
    (when ensurer
      (trc (and *c-debug* (> *data-pulse-id* 495)(trcp c))
        "ensure.value-is-current > entry2" 
        :ensurer (qci ensurer)))
    
    (when *not-to-be*
      (when (c-unboundp c)
        (error 'unbound-cell :cell c :instance (c-model c) :name (c-slot-name c)))
      (return-from ensure-value-is-current
        (when (c-validp c) ;; probably accomplishes nothing
          (c-value c))))
    
    (when (and (not (symbolp (c-model c))) ;; damn, just here because of playing around with global vars and cells
            (eq :eternal-rest (md-state (c-model c))))
      (break "model ~a of cell ~a is dead" (c-model c) c))
    
    (cond
     ((c-currentp c)
      (count-it! :ensvc-is-indeed-currentp)
      (trc nil "EVIC yep: c-currentp" c)
      ) ;; used to follow c-inputp, but I am toying with letting ephemerals (inputs) fall obsolete
     ;; and then get reset here (ie, ((c-input-p c) (ephemeral-reset c))). ie, do not assume inputs are never obsolete
     ;;
     ((and (c-inputp c)
        (c-validp c) ;; a c?n (ruled-then-input) cell will not be valid at first
        (not (and (typep c 'c-dependent)
               (eq (cd-optimize c) :when-value-t)
               (null (c-value c)))))
      (trc nil "evic: cool: inputp" (qci c)))
     
     ((or (bwhen (nv (not (c-validp c)))
            (count-it! :ens-val-not-valid)
            (trc nil "not c-validp, gonna run regardless!!!!!!" c)
            nv)
        ;;
        ;; new for 2006-09-21: a cell ended up checking slots of a dead instance, which would have been
        ;; refreshed when checked, but was going to be checked last because it was the first used, useds
        ;; being simply pushed onto a list as they come up. We may need fancier handling of dead instance/cells
        ;; still being encountered by consulting the prior useds list, but checking now in same order as
        ;; accessed seems Deeply Correct (and fixed the immediate problem nicely, always a Good Sign).
        ;;
        (labels ((check-reversed (useds)
                   (when useds
                     (or (check-reversed (cdr useds))
                       (let ((used (car useds)))
                         (ensure-value-is-current used :nested c)
                         #+slow (trc nil "comparing pulses (ensurer, used, used-changed): "  c debug-id used (c-pulse-last-changed used))
                         (when (> (c-pulse-last-changed used)(c-pulse c))
                           (count-it! :ens-val-someused-newer)
                           (trc nil "used changed and newer !!!!######!!!!!! used" (qci used) :oldpulse (c-pulse used)
                             :lastchg (c-pulse-last-changed used))
                           #+shhh (when (trcp c)
                                    (describe used))
                           t))))))
          (assert (typep c 'c-dependent))
          (check-reversed (cd-useds c))))
      (trc nil "kicking off calc-set of!!!!" (c-state c) (c-validp c) (qci c) :vstate (c-value-state c)
        :stamped (c-pulse c) :current-pulse *data-pulse-id*)
      (calculate-and-set c :evic ensurer)
      (trc nil "kicked off calc-set of!!!!" (c-state c) (c-validp c) (qci c) :vstate (c-value-state c)
        :stamped (c-pulse c) :current-pulse *data-pulse-id*))
     
     #+bahhhh ;; 2009-12-27 not sure on this. Idea is to keep the dead around as static data. Might be better not to let data go dead
     ((mdead (c-value c))
      (trc  "ensure.value-is-current> trying recalc of ~a with current but dead value ~a" c (c-value c))
      (let ((new-v (calculate-and-set c :evic-mdead ensurer)))
        (trc  "ensure.value-is-current> GOT new value ~a to replace dead!!" new-v)
        new-v))
     
     (t (trc nil "ensure.current decided current, updating pulse" (c-slot-name c) debug-id)
       (c-pulse-update c :valid-uninfluenced)))
    
    (when (c-unboundp c)
      (error 'unbound-cell :cell c :instance (c-model c) :name (c-slot-name c)))
    
    (bwhen (v (c-value c))
      #-its-alive!
      (when (mdead v)
        #+shhh (format t "~&on pulse ~a cell ~a value dead ~a" *data-pulse-id* c v))
      v)))


(defun calculate-and-set (c dbgid dbgdata)
  (declare (ignorable dbgid dbgdata)) ;; just there for inspection of the stack during debugging
  (flet ((body ()
           (when (c-stopped)
             (princ #\.)
             (return-from calculate-and-set))

           #-its-alive!
           (bwhen (x (find c *call-stack*)) ;; circularity
             (unless nil ;; *stop*
               (let ()
                 (inspect c)
                 (trc "calculating cell:" c (cr-code c))
                 (trc "appears-in-call-stack (newest first): " (length *call-stack*))
                 (loop for caller in (copy-list *call-stack*)
                     for n below (length *call-stack*)
                     do (trc "caller> " caller #+shhh (cr-code caller))
                       when (eq caller c) do (loop-finish))))
             (setf *stop* t)  
             (c-break ;; break is problem when testing cells on some CLs
              "cell ~a midst askers (see above)" c)
             (error 'asker-midst-askers :cell c))
  
           (multiple-value-bind (raw-value propagation-code)
               (calculate-and-link c)
             
             (when (and *c-debug* (typep raw-value 'cell))
               (c-break "new value for cell ~s is itself a cell: ~s. probably nested (c? ... (c? ))"
                 c raw-value))
             
             (unless (c-optimized-away-p c)
               ; this check for optimized-away-p arose because a rule using without-c-dependency
               ; can be re-entered unnoticed since that clears *call-stack*. If re-entered, a subsequent
               ; re-exit will be of an optimized away cell, which we need not sv-assume on... a better
               ; fix might be a less cutesy way of doing without-c-dependency, and I think anyway
               ; it would be good to lose the re-entrance.
               (md-slot-value-assume c raw-value propagation-code)))))
    (if (trcp c) ;; *dbg*
        (wtrc (0 100 "calcnset" c) (body))
      (body))))

(defun calculate-and-link (c)
  (let ((*call-stack* (cons c *call-stack*))
        (*depender* c)
        (*defer-changes* t))
    (assert (typep c 'c-ruled))
    (trc nil "calculate-and-link" c)
    (cd-usage-clear-all c)
    (multiple-value-prog1
        (funcall (cr-rule c) c)
      (c-unlink-unused c))))


;-------------------------------------------------------------

(defun md-slot-makunbound (self slot-name
                            &aux (c (md-slot-cell self slot-name)))
  (unless c
    (c-break ":md-slot-makunbound > cellular slot ~a of ~a cannot be unbound unless initialized as inputp"
      slot-name self))
  
  (when (c-unboundp c)
    (return-from md-slot-makunbound nil))

  (when *within-integrity* ;; 2006-02 oops, bad name
    (c-break "md-slot-makunbound of ~a must be deffered by wrapping code in with-integrity" c))
  
  ; 
  ; Big change here for Cells III: before, only the propagation was deferred. Man that seems
  ; wrong. So now the full makunbound processing gets deferred. Less controversially,
  ; by contrast the without-c-dependency wrapped everything, and while that is harmless,
  ; it is also unnecessary and could confuse people trying to follow the logic.
  ;
  (let ((causation *causation*))
    (with-integrity (:change c)
      (let ((*causation* causation))
        ; --- cell & slot maintenance ---
        (let ((prior-value (c-value c)))
          (setf (c-value-state c) :unbound
            (c-value c) nil
            (c-state c) :awake)
          (bd-slot-makunbound self slot-name)
          ;
           ; --- data flow propagation -----------
          ;
          (without-c-dependency
              (c-propagate c prior-value t)))))))

;;; --- setf md.slot.value --------------------------------------------------------
;;;

(defun (setf md-slot-value) (new-value self slot-name
                              &aux (c (md-slot-cell self slot-name)))
  #+shhh (when *within-integrity*
    (trc "mdsetf>" self (type-of self) slot-name :new new-value))
  (when *c-debug*
    (c-setting-debug self slot-name c new-value))
  
  (unless c
    (c-break "cellular slot ~a of ~a cannot be SETFed because it is not 
mediated by a Cell with :inputp t. To achieve this, the initial value ~s -- whether 
supplied as an :initform, :default-initarg, or at make-instance time via 
an :initarg -- should be wrapped in either macro C-IN or C-INPUT. 
In brief, initialize ~0@*~a to (c-in ~2@*~s) instead of plain ~:*~s"
      slot-name self (slot-value self slot-name)))

  (cond
   ((find (c-lazy c) '(:once-asked :always t))
    (md-slot-value-assume c new-value nil))

   (*defer-changes*
    (c-break "SETF of ~a must be deferred by wrapping code in WITH-INTEGRITY" c))

   (t
    (with-integrity (:change slot-name)
      (md-slot-value-assume c new-value nil))))

  ;; new-value 
  ;; above line commented out 2006-05-01. It seems to me we want the value assumed by the slot
  ;; not the value setf'ed (on rare occasions they diverge, or at least used to for delta slots)
  ;; anyway, if they no longer diverge the question of which to return is moot
  )
                    
(defun md-slot-value-assume (c raw-value propagation-code)
  (assert c)
  (trc nil "md-slot-value-assume entry" (qci c)(c-state c))
  (without-c-dependency
      (let ((prior-state (c-value-state c))
            (prior-value (c-value c))
            (absorbed-value (c-absorb-value c raw-value)))

        (c-pulse-update c :slotv-assume)

        ; --- head off unchanged; this got moved earlier on 2006-06-10 ---
        (when (and (not (eq propagation-code :propagate))
                (find prior-state '(:valid :uncurrent))
                (c-no-news c absorbed-value prior-value))
          (setf (c-value-state c) :valid) ;; new for 2008-07-15
          (trc nil "(setf md-slot-value) > early no news" propagation-code prior-state prior-value  absorbed-value)
          (count-it :nonews)
          (return-from md-slot-value-assume absorbed-value))

        ; --- slot maintenance ---
        
        (unless (c-synaptic c) 
          (md-slot-value-store (c-model c) (c-slot-name c) absorbed-value))
        
        ; --- cell maintenance ---
        (setf
         (c-value c) absorbed-value
         (c-value-state c) :valid
         (c-state c) :awake)
        
        (case (and (typep c 'c-dependent)
                   (cd-optimize c))
          ((t) (c-optimize-away?! c)) ;;; put optimize test here to avoid needless linking
          (:when-value-t (when (c-value c)
                           (c-unlink-from-used c))))
        
        ; --- data flow propagation -----------
        (unless (eq propagation-code :no-propagate)
          (trc nil "md-slot-value-assume flagging as changed: prior state, value:" prior-state prior-value )
          (c-propagate c prior-value (cache-state-bound-p prior-state)))  ;; until 06-02-13 was (not (eq prior-state :unbound))
        (trc nil "exiting md-slot-val-assume" (c-state c) (c-value-state c))
        absorbed-value)))

(defun cache-bound-p (c)
  (cache-state-bound-p (c-value-state c)))

(defun cache-state-bound-p (value-state)
  (or (eq value-state :valid)
    (eq value-state :uncurrent)))

;---------- optimizing away cells whose dependents all turn out to be constant ----------------
;

(defun flushed? (c)
  (rassoc c (cells-flushed (c-model c))))

(defun c-optimize-away?! (c)
  #+shhh (trc nil "c-optimize-away?! entry" (c-state c) c)
  (when (and (typep c 'c-dependent)
          (null (cd-useds c))
          (cd-optimize c)
          (not (c-optimized-away-p c)) ;; c-streams (FNYI) may come this way repeatedly even if optimized away
          (c-validp c) ;; /// when would this not be the case? and who cares?
          (not (c-synaptic c)) ;; no slot to cache invariant result, so they have to stay around)
          (not (c-inputp c)) ;; yes, dependent cells can be inputp
          )
    ;; (when (trcp c) (break "go optimizing ~a" c))
    
    (when (trcp c)
      (trc "optimizing away" c (c-state c) (rassoc c (cells (c-model c)))(rassoc c (cells-flushed (c-model c))))
      )

    (count-it :c-optimized)
    
    (setf (c-state c) :optimized-away)
    
    (let ((entry (rassoc c (cells (c-model c)))))
      (unless entry
        (describe c)
        (bwhen (fe (rassoc c (cells-flushed (c-model c))))
          (trc "got in flushed thoi!" fe)))
      (c-assert entry)
      ;(trc (eq (c-slot-name c) 'cgtk::id) "c-optimize-away?! moving cell to flushed list" c)
      (setf (cells (c-model c)) (delete entry (cells (c-model c))))
      #-its-alive! (push entry (cells-flushed (c-model c)))
      )
    
    (dolist (caller (c-callers c) )
      ;
      ; example: on window shutdown with a tool-tip displayed, the tool-tip generator got
      ; kicked off and asked about the value of a dead instance. That returns nil, and
      ; there was no other dependency, so the Cell then decided to optimize itself away.
      ; of course, before that time it had a normal value on which other things depended,
      ; so we ended up here. where there used to be a break.
      ;
      (setf (cd-useds caller) (delete c (cd-useds caller)))
      ;;; (trc "nested opti" c caller)
      (c-optimize-away?! caller) ;; rare but it happens when rule says (or .cache ...)
      )))

    
