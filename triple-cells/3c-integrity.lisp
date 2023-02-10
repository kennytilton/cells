(in-package :3c)

(defmacro with-3c-integrity ((&optional opcode defer-info debug) &rest body)
  `(call-with-3c-integrity ,opcode ,defer-info (lambda (opcode defer-info)
                                                 (declare (ignorable opcode defer-info))
                                                 ,(when debug
                                                    `(trc "integrity action entry" opcode defer-info ',body))
                                                 ,@body)))

(defmacro with-3cc (id &body body)
  `(with-3c-integrity (:change ,id)
     ,@body))

(defun 3c-integrity-managed? ()
  (get-triple :s !ccc:integrity :p !ccc:within))

(defun (setf 3c-integrity-managed?) (on?)
  (if on?
      (if (get-triple :s !ccc:integrity :p !ccc:within)
        (break "integ already managed")
        (add-triple !ccc:integrity !ccc:within (new-blank-node)))
    (bif (tr (get-triple :s !ccc:integrity :p !ccc:within))
      (delete-triple (triple-id tr))
      (warn "integ not being managed, nothing to turn off"))))

(defun call-with-3c-integrity (opcode defer-info action)
  (if (3c-integrity-managed?)
      (if opcode
          (3c-ufb-add opcode defer-info)
          (funcall action opcode defer-info))
    (prog2
      (setf (3c-integrity-managed?) t)

        (progn ;; let (*defer-changes*)
          (when (or (null (3c-pulse))
                  (eq opcode :change))
            (3c-pulse-advance (cons opcode defer-info)))
          (prog1
              (funcall action opcode defer-info)
            (3c-finish-business)))

      (setf (3c-integrity-managed?) nil))))

(defun 3c-ufb-add (opcode defer-info)
  (add-triple opcode (mk-upi (get-internal-real-time)) defer-info))

(defun 3c-finish-business ()
  (tagbody
    tell-dependents
    (process-tell-dependents)
    (process-awaken)
    (when (get-triple :p !ccc:tell-dependents)
      (go tell-dependents))
    
;;;    ;--- process client queue ------------------------------
;;;    ;
;;;    handle-clients
;;;    (bwhen (clientq (ufb-queue :client))
;;;      (if *client-queue-handler*
;;;          (funcall *client-queue-handler* clientq) ;; might be empty/not exist, so handlers must check
;;;        (just-do-it clientq))
;;;      (when (fifo-peek (ufb-queue :client))
;;;        #+shhh (ukt::fifo-browse (ufb-queue :client) (lambda (entry)
;;;                                                       (trc "surprise client" entry)))
;;;        (go handle-clients)))
    (process-reset-ephemerals)

;;;    (bwhen (task-info (fifo-pop (ufb-queue :change)))
;;;      (trc nil "!!! finbiz --- CHANGE ---- (first of)" (fifo-length (ufb-queue :change)))
;;;      (destructuring-bind (defer-info . task-fn) task-info
;;;        (trc nil  "finbiz: deferred state change" defer-info)
;;;        (data-pulse-next (list :finbiz defer-info))
;;;        (funcall task-fn :change defer-info)
;;;        (go tell-dependents)))
    ))

(defun process-tell-dependents ()
  (index-new-triples)
  (loop while (loop with any 
                  for cell in (prog1
                                  (mapcar 'object (get-triples-list :s !ccc:ufb-tell-dependents))
                                (delete-triples :s !ccc:ufb-tell-dependents))
                  do (loop for user in (get-triples-list :p !ccc:uses :o cell)
                         do (trc nil "propagating !!!!!!!!!!!!" cell :to (cell-predicate (subject user)))
                           (setf any t)
                           (3c-ensure-current (subject user)))
                  finally (return any))))

(defun process-awaken ()
  (index-new-triples)
  (loop for cell in (prog1
                        (mapcar 'object (get-triples-list :s !ccc:awaken-ruled-cell))
                      (delete-triples :s !ccc:awaken-ruled-cell))
      do (3c-awaken-ruled-cell cell))
  (loop for o in (prog1
                     (mapcar 'object (get-triples-list :s !ccc:observe))
                   (delete-triples :s !ccc:observe))
      do (if (3c-cell? o)
             (cell-observe-change o (cell-model o) (cell-predicate o) (3c-cell-value o) nil nil)
           (let ((tr (get-triple-by-id (upi->value o)))) ;; must be a mod-pred-triple constant
             (trc "obsing k" tr (predicate tr))
             (cell-observe-change nil (subject tr) (predicate tr) (upi->value (object tr)) nil nil)))))

(defun process-reset-ephemerals ()
  (let ((q !ccc:ufb-reset-ephemerals))
    (index-new-triples)
    (loop for cell in (prog1
                          (mapcar 'object (get-triples-list :s q))
                        (delete-triples :s q))
        for p = (cell-predicate cell)
        do ;(trc "resetting ephemeral" p)
          (delete-triples :s cell :p !ccc:value)
          (delete-triples :s (cell-model cell) :p p))))