;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
;;;
;;; Copyright (c) 1995,2003 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.


(in-package :cells)

(defvar *name-ct-calc* 0)

(defmodel person ()
  ((speech :cell :ephemeral :initform (c-in nil) :initarg :speech :accessor speech)
   (thought :cell :ephemeral :initform (c? (speech self)) :initarg :thought :accessor thought)
   (names :initform nil :initarg :names :accessor names)
   (pulse :initform nil :initarg :pulse :accessor pulse)
   (name-ct :initarg :name-ct :accessor name-ct
            :initform (c? "name-ct" 
                          (incf *name-ct-calc*)
                          (length (names self))))))

(defobserver names ((self person) new-names)
  (format t "~&you can call me ~a" new-names))

(defmethod c-unchanged-test ((self person) (slotname (eql 'names)))
  'equal)

(defvar *thought* "failed")
(defvar *output-speech* "failed")

(defobserver thought ((self person) new-value)
  (when new-value
    (trc "output thought" self new-value)
    (setq *thought* new-value)
    (trc "i am thinking" new-value)))

(defobserver speech ()
  (setf *output-speech* new-value))

(defmodel sick ()
  ((e-value :cell :ephemeral :initarg :e-value :accessor e-value)
   (s-value :initarg :s-value :reader s-value)))

(defobserver s-value () 
  :test)

(defobserver e-value () 
  :test)

(def-cell-test cv-test-person ()
  (cv-test-person-1)
  (cv-test-person-3)
  (cv-test-person-4)
  (cv-test-person-5)
  ;; (cv-test-talker)
  )

(def-cell-test cv-test-person-1 ()
  ;; 
  ;; a recent exchange with someone who has developed with others a visual
  ;; programming system was interesting. i mentioned my dataflow thing, he mentioned
  ;; they liked the event flow model. i responded that events posed a problem for
  ;; cells. consider something like:
  ;;
  ;; (make-instance 'button
  ;;      :clicked (c-in nil)
  ;;      :action (c? (when (clicked self) (if (- (time-now *cg-system*) (last-click-time.....
  ;;
  ;; well, once the button is clicked, that cell has the value t. the rest of the rule executes
  ;; and does whatever, the rule completes. finis? no. the time-now cell of
  ;; the system instance continues to tick-tick-tick. at each tick the action cell gets triggered,
  ;; and (here is the problem) the clicked cell still says t.
  ;;
  ;; the problem is that clicked is event-ish. the semantics are not "has it ever been clicked",
  ;; they are more like "when the /passing/ click occurs...". we could try requiring the programmer
  ;; always to execute:
  ;;
  ;;     (setf (clicked it) t)
  ;;     (setf (clicked it nil)
  ;;
  ;; ...but in fact cells like this often are ruled cells which watch mouse actions and check if the
  ;; mouse up was in the control where the mousedown occurred. so where to put a line of code
  ;; to change clicked back to nil? a deep fix seemed appropriate: teach cells about events, so...
  ;;
  ;; cellular slots can be defined to be :ephemeral if the slot will be used for
  ;; event-like data. [defining slots and not cells as ephemeral means one cannot arrange for such a 
  ;; slot to have a non-ephemeral value for one instance and ephemeral values for other instances. we 
  ;; easily could go the other way on this, but this seems right.] 
  ;;
  ;; the way ephemerals work is this: when a new value arrives in an ephemeral slot it is outputted and 
  ;; propagated to dependent cells normally, but then internally the slot value is cleared to nil.
  ;; thus during the output and any dataflow direct or indirect the value is visible to other code, but
  ;; no longer than that. note that setting the slot back to nil bypasses propagation: no output, no 
  ;; triggering of slot dependents.
  ;;
  ;;
  (let ((p (make-instance 'person :speech (c-in nil))))
    ;;
    ;; - ephemeral c-variable cells revert to nil if setf'ed non-nil later
    ;;
    (setf (speech p) "thanks for all the fish")
    (ct-assert (null (speech p)))
    (ct-assert (equal *output-speech* "thanks for all the fish"))
    (ct-assert (equal *thought* "thanks for all the fish")) ;; thought is ephemeral as well, so tricky test
    ;;
    ;; now check the /ruled/ ephemeral got reset to nil
    ;;
    (ct-assert (null (thought p)))))



(def-cell-test cv-test-person-3 ()
  ;; -------------------------------------------------------
  ;;  dynamic dependency graph maintenance
  ;;
  ;; dependencies of a cell are those other cells actually accessed during the latest
  ;; invocation of the rule. note that a cellular slot may be constant, not mediated by a
  ;; cell, in which case the access does not record a dependency.
  ;;
  (let ((p (make-instance 'person
             :names (c-in '("speedy" "chill"))
             :pulse (c-in 60)
             :speech "nice and easy does it"
             :thought (c? (if (> (pulse self) 180)
                              (concatenate 'string (car (names self)) ", slow down!")
                            (speech self))))))
    ;;
    ;; with the (variable=1) pulse not > 80, the branch taken leads to (constant=0) speech, so:
    ;;
    (ct-assert (eql 1 (length (cd-useds (md-slot-cell p 'thought)))))
    ;;
    ;; with the (variable=1) pulse > 80, the branch taken leads to (variable=1) names, so:
    ;;
    (setf (pulse p) 200)
    (ct-assert (eql 2 (length (cd-useds (md-slot-cell p 'thought)))))
    ;;
    ;; let's check the engine's ability reliably to drop dependencies by lowering the pulse again
    ;;
    (setf (pulse p) 50)
    (ct-assert (eql 1 (length (cd-useds (md-slot-cell p 'thought)))))))


(def-cell-test cv-test-person-4 ()
  (let ((p (make-instance 'person
             :names '("speedy" "chill")
             :pulse (c-in 60)
             :speech (c? (car (names self)))
             :thought (c? (when (< (pulse self) 100) (speech self))))))
    ;;
    ;; now let's see if cells are correctly optimized away when:
    ;;
    ;;    - they are defined and
    ;;    - all cells accessed are constant.
    ;;
    (ct-assert (null (md-slot-cell p 'speech)))
    (ct-assert (assoc 'speech (cells-flushed  p)))
    #+its-alive! (ct-assert (numberp (cdr (assoc 'speech (cells-flushed  p)))))
    #-its-alive! (ct-assert (c-optimized-away-p (cdr (assoc 'speech (cells-flushed  p)))))
    ;;(inspect p)
    (ct-assert (not (c-optimized-away-p (md-slot-cell p 'thought)))) ;; pulse is variable, so cannot opti
    (ct-assert (eql 1 (length (cd-useds (md-slot-cell p 'thought))))) ;; but speech is opti, so only 1 used
    ))

(def-cell-test cv-test-person-5 ()
  ;;
  ;; for now cells do not allow cyclic dependency, where a computation of a cell leads back
  ;; to itself. we could do something like have the self-reference return the cached value
  ;; or (for the first evaluation) a required seed value. we already have logic which says
  ;; that, if setf on a variable cell cycles back to setf on the same cell we simply stop, so
  ;; there is no harm on the propagation side. but so far no need for such a thing.
  ;;
  ;; one interesting experiment would be to change things so propagation looping back on itself
  ;; would be allowed. we would likewise change things so propagation was breadth first. then
  ;; state change, once set in motion, would continue indefinitely. (propagation would also have to
  ;; be non-recursive.) we would want to check for os events after each propagation and where
  ;; real-time synchronization was necessary do some extra work. this in contrast to having a timer 
  ;; or os null events artificially move forward the state of, say, a simulation of a physical system. 
  ;; allowing propagation to loop back on itslef means the system would simply run, and might make
  ;; parallelization feasible since we already have logic to serialize where semantically necessary.
  ;; anyway, a prospect for future investigation.
  ;;
  ;;   make sure cyclic dependencies are trapped:
  ;;
  (cells-reset)
  (ct-assert
   (handler-case
       (progn
         (pulse (make-instance 'person
                  :names (c? (trc "calculating names" self)
                           (maptimes (n (pulse self))))
                  :pulse (c? (trc "calculating pulse" self)
                           (length (names self)))))
         nil)
     (asker-midst-askers (c)
       (print `(:cool-asker-midst-askers-thrown! ,c)))
     (t (error)
        (describe  error)
       (setf *stop* t)
       (break)
        t)))
  )
;;
;; we'll toss off a quick class to test tolerance of cyclic

(defmodel talker8 ()
  ((words8 :initform (c-input (:cyclicp t) "hello, world")
     :initarg :words8 :accessor words8)
   (idea8 :initform (c-in "new friend!") :initarg :idea8 :accessor idea8)
   (mood8 :initform (c-in "happy as clam") :initarg :mood8 :accessor mood8)))

(defmodel talker ()
  ((words :initform (c-in "hello, world") :initarg :words :accessor words)
   (idea :initform (c-in "new friend!") :initarg :idea :accessor idea)
   ))

(defobserver words ((self talker) new-words)
  (trc "new words" new-words)
  (setf (idea self) (concatenate 'string "idea " new-words)))

(defmethod c-unchanged-test ((self talker) (slotname (eql 'words)))
  'string-equal)

(defobserver idea ((self talker) new-idea)
  (trc "new idea" new-idea)
  (setf (words self) (concatenate 'string "say " new-idea)))

(defmethod c-unchanged-test ((self talker) (slotname (eql 'idea)))
  'string-equal)

(defobserver words8 ((self talker8) new-words8)
  (trc "new words8, sets idea8 to same" new-words8 *causation*)
  (with-integrity (:change)
      (setf (idea8 self) (concatenate 'string "+" new-words8))))

(defmethod c-unchanged-test ((self talker8) (slotname (eql 'words8)))
  'string-equal)

(defobserver idea8 ((self talker8) new-idea8)
  (trc "new idea8, sets mood8 to same" new-idea8 *causation*)
  (with-integrity (:change)
      (setf (mood8 self) (concatenate 'string "+" new-idea8))))

(defmethod c-unchanged-test ((self talker8) (slotname (eql 'idea8)))
  'string-equal)

(defobserver mood8 ((self talker8) new-mood8)
  (trc "new mood8, sets words8 to same:" new-mood8 *causation*)
  (with-integrity (:change)
      (setf (words8 self) (concatenate 'string "+" new-mood8))))

(defmethod c-unchanged-test ((self talker8) (slotname (eql 'mood8)))
  'string-equal)

(defmacro ct-assert-error (&body body)
  `(ct-assert
    (handler-case
        (prog1 nil
          ,@body)
     (t (error)
        (trc "ct-assert-error" error)
       (setf *stop* nil)
        t))))

#+(or) ; FIXME: this test is borked
(def-cell-test cv-test-talker ()
  ;;
  ;; make sure cyclic setf is trapped
  ;;
  (cells-reset)
  
  ;;;  (trc "start unguarded cyclic")
  ;;;
  ;;;  (let ((tk (make-instance 'talker)))
  ;;;     (setf (idea tk) "yes")
  ;;;     (string-equal "yes" (words tk))
  ;;;     (setf (words tk) "no")
  ;;;     (string-equal "no" (idea tk)))
  
  (trc "start guarded cyclic")
  
  #+(or) (ct-assert-error
         (let ((tk (make-instance 'talker)))
           (setf (idea tk) "yes")
           (ct-assert (string-equal "yes" (words tk)))
           (setf (words tk) "no")
           (ct-assert (string-equal "no" (idea tk)))))
  ;;
  ;; make sure cells declared to be cyclic are allowed
  ;; and halt (because after the first cyclic setf the cell in question
  ;; is being given the same value it already has, and propagation stops.
  ;;
  (make-instance 'talker8)
  #+(or) (let ((tk (make-instance 'talker8)))
          (setf (idea8 tk) "yes")
          (string-equal "yes" (words8 tk))
          (setf (words8 tk) "no")
          (string-equal "no" (idea8 tk)))
  )
