;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells)


(define-constant *ufb-opcodes* '(:tell-dependents
                                 :awaken
                                 :client
                                 :ephemeral-reset
                                 :change
                                 :post-change))

(defmacro with-integrity ((&optional opcode defer-info debug) &rest body)
  (declare (ignorable debug))
  (when opcode
    (assert (find opcode *ufb-opcodes*) ()
      "Invalid opcode for with-integrity: ~a. Allowed values: ~a" opcode *ufb-opcodes*))
  `(call-with-integrity ,opcode ,defer-info
     (lambda (opcode defer-info)
       (declare (ignorable opcode defer-info))
       ;;;       ,(when debug
       ;;;          `(trc "integrity action entry" opcode defer-info ',body))
       ;;;       (when *c-debug*
       #+shhh (when (eq opcode :change)
         (trc "-------w/integ :change go--------------->:" defer-info))
       ,@body)
     nil
     #+noway (when *c-debug* ',body)))

(export! with-cc without-integrity)

(defmacro with-cc (id &body body)
  `(with-integrity (:change ,id)
     ;;(print `(with-cc-fires!!!!!!!!!! ,',id))
     ,@body))

(defun integrity-managed-p ()
  *within-integrity*)

(defmacro without-integrity (&body body)
  `(let (*within-integrity* *defer-changes* *call-stack*)
     ,@body))

(defun call-with-integrity (opcode defer-info action code)
  (declare (ignorable code))
  (when *stop*
    (print :cwi-sees-stop!!!!!!!!!!!)
    (return-from call-with-integrity))
  (if *within-integrity*
      (if opcode
          (prog1
              :deferred-to-ufb-1 ; SETF is supposed to return the value being installed
            ; in the place, but if the SETF is deferred we return
            ; something that will help someone who tries to use
            ; the setf'ed value figure out what is going on:
            (ufb-add opcode (cons defer-info action)))

        ; thus by not supplying an opcode one can get something
        ; executed immediately, potentially breaking data integrity
        ; but signifying by having coded the with-integrity macro
        ; that one is aware of this. If you have read this comment.
        (funcall action opcode defer-info))

    (flet ((go-go ()
             (let ((*within-integrity* t)
                   *unfinished-business*
                   *finbiz-id*
                   *defer-changes*)
               (trc nil "initiating new UFB!!!!!!!!!!!!" opcode defer-info)
               ;(when *c-debug* (assert (boundp '*istack*)))
               (when (or (zerop *data-pulse-id*)
                       (eq opcode :change))
                 (eko (nil "!!! New pulse, event" *data-pulse-id* defer-info)
                   (data-pulse-next (cons opcode defer-info))))
               (prog1
                   (funcall action opcode defer-info)
                 (let ((*finbiz-id* 0))
                   ;(print :finishing-business)
                   (finish-business)
                   (assert-ufb-all-empty)
                   ;; (assert-ufb-q-empty :tell-dependents)
                   ;; (assert-ufb-q-empty :change)
                   )))))
      (if nil ;; *c-debug*
          (let ((*istack* (list (list opcode defer-info)
                            (list :trigger code)
                            (list :start-dp *data-pulse-id*))))
            (trc "*istack* bound")
            (handler-case
                (go-go)
              (xcell (c)
                (if (functionp *c-debug*)
                    (funcall *c-debug* c (nreverse *istack*))
                  (loop for f in (nreverse *istack*)
                      do (format t "~&istk> ~(~a~) " f)
                      finally (describe c)
                         (brk "integ backtrace: see listener for deets")))))
            (trc "*istack* unbinding"))
        (go-go)))))

(defun ufb-queue (opcode)
  (cdr (assoc opcode *unfinished-business*)))

(defun ufb-queue-ensure (opcode)
  #-live (assert (find opcode *ufb-opcodes*))
  (or (ufb-queue opcode)
    (cdr (car (push (cons opcode (make-fifo-queue)) *unfinished-business*)))))

(defparameter *no-tell* nil)

(defun ufb-add (opcode continuation)
  #+trythis (when (and *no-tell* (eq opcode :tell-dependents))
    (brk "truly queueing tell under no-tell"))
  (trc nil "ufb-add deferring" opcode (when (eql opcode :client)(car continuation)))
  (fifo-add (ufb-queue-ensure opcode) continuation))

(defparameter *just-do-it-q* nil)

(defun just-do-it (op-or-q &optional (op-code op-or-q) ;; [mb]
                    &aux (q (if (keywordp op-or-q)
                                (ufb-queue op-or-q)
                              op-or-q)))
  (declare (ignorable op-code))
  (let ((*just-do-it-q* op-or-q)) ;; debug aid
    (trc nil "----------------------------just do it doing---------------------" op-or-q)
    (loop for (defer-info . task) = (fifo-pop q)
        while task
        do (trc nil "unfin task is" opcode task)
          #+chill (when *c-debug*
                    (push (list op-code defer-info) *istack*))
          (funcall task op-or-q defer-info))))

(defun assert-ufb-all-empty ()
  #-live
  (loop for (opcode . nil) in *unfinished-business*
      do (assert-ufb-q-empty opcode)))

(defun assert-ufb-q-empty (q)
  (declare (ignorable q))
  #-live
  (bwhen (uqp (fifo-peek (ufb-queue q)))
    (print `(assert-ufb-q-empty-fail ,q ,uqp))
    (dolist (b (fifo-data (ufb-queue q)))
      (trc "assert-ufb-q-empty sees unhandled elt" b q)
      ;; (trc "unhandled" q (car b) (c-callers (car b)))
      )
    (brk "unexpected ufb> ufb ~a remain" q)))

(defun finish-business ()
  (when *stop* (return-from finish-business))
  (incf *finbiz-id*)
  (tagbody
    tell-dependents
    (just-do-it :tell-dependents)
    ;
    ; while the next step looks separate from the prior, they are closely bound.
    ; during :tell-dependents, any number of new model instances can be spawned.
    ; as they are spawned, shared-initialize queues them for awakening, which
    ; you will recall forces the calculation of ruled cells and observer notification
    ; for all cell slots. These latter may enqueue :change or :client tasks, in which
    ; case note that they become appended to :change or :client tasks enqueued
    ; during :tell-dependents. How come? Because the birth itself of model instances during
    ; a datapulse is considered part of that datapulse, so we do want tasks enqueued
    ; during their awakening to be handled along with those enqueued by cells of
    ; existing model instances.
    ;
    (assert-ufb-q-empty :tell-dependents)
    (let ((*no-tell* t))
      (just-do-it :awaken) ;--- md-awaken new instances ---
      )
    ;
    ;
    ; We now allow :awaken to change things so more dependents need to be told. The problem is the implicit 
    ; dependence on the /life/ of a model whenever there is a dependence on any /cell/ of that model. 
    ; md-quiesce currently just flags such slots as uncurrent -- maybe /that/ should change and those should 
    ; recalculate at once -- and then an /observer/ can run and ask for a new value from such an uncurrent cell, 
    ; which now knows it must recalculate. And that recalculation of course can and likely will come up with a new value
    ; and perforce need to tell its dependents. So...
    ;
    ; I /could/ explore something other than the "uncurrent" kludge, but NCTM 2007 is coming up and
    ; to be honest the idea of not allowing nested tells was enforcing a /guess/ that that should not
    ; arise, and there was not even any perceived integrity whole being closed, it was just a gratuitous
    ; QA trick, and indeed for a long time many nested tells were avoidable. But the case of the quiesced
    ; dependent reverses the arrow and puts the burden on the prosecution to prove nested tells are a problem.
    
    (bwhen (uqp (fifo-peek (ufb-queue :tell-dependents)))
      #+xxx (trc "retelling dependenst, one new one being" uqp)
      (go tell-dependents))
    
    ;--- process client queue ------------------------------
    ;
    (when *stop* (return-from finish-business))
    
    handle-clients
    (bwhen (clientq (ufb-queue :client))
      (if *client-queue-handler*
          (funcall *client-queue-handler* clientq) ;; might be empty/not exist, so handlers must check
        (just-do-it clientq :client))
      (when (fifo-peek (ufb-queue :client))
        #+shhh (ukt::fifo-browse (ufb-queue :client) (lambda (entry)
                                                       (trc "surprise client" entry)))
        (go handle-clients)))
    ;--- now we can reset ephemerals --------------------
    ;
    ; one might be wondering when the observers got notified. That happens right during
    ; slot.value.assume, via c-propagate.
    ;
    ; Nice historical note: by accident, in the deep-cells test to exercise the new behavior
    ; of cells3, I coded an ephemeral cell and initialized it to non-nil, hitting a runtime
    ; error (now gone) saying I had no idea what a non-nil ephemeral would mean. That had been
    ; my conclusion when the idea occurred to me the first time, so I stuck in an assertion
    ; to warn off callers. 
    ;
    ; But the new
    ; datachange progression defined by Cells3 had already forced me to manage ephemeral resets
    ; more predictably (something in the test suite failed). By the time I got the runtime
    ; error on deep-cells I was able to confidently take out the error and just let the thing
    ; run. deep-cells looks to behave just right, but maybe a tougher test will present a problem?
    ;
    (just-do-it :ephemeral-reset)
    
    ;--- do deferred state changes -----------------------
    ;
    (bwhen (task-info (fifo-pop (ufb-queue :change)))
      (trc nil "!!! finbiz --- CHANGE ---- (first of)" (fifo-length (ufb-queue :change)))
      (destructuring-bind (defer-info . task-fn) task-info
        (trc nil "fbz: dfrd chg" defer-info (fifo-length (ufb-queue :change)))
        (data-pulse-next (list :finbiz defer-info))
        (funcall task-fn :change defer-info)
        ;
        ; to finish this state change we could recursively call (finish-business), but
        ; a goto let's us not use the stack. Someday I envision code that keeps on
        ; setf-ing, polling the OS for events, in which case we cannot very well use
        ; recursion. But as a debugger someone might want to change the next form
        ; to (finish-business) if they are having trouble with a chain of setf's and
        ; want to inspect the history on the stack.
        ;
        (go tell-dependents)))))


