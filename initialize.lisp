;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton



|#

(in-package :cells)

(eval-when (compile eval load)
  (export '(c-envalue)))

(defstruct (c-envaluer (:conc-name nil))
  envalue-rule)

(defmethod awaken-cell (c)
  (declare (ignorable c)))

(defmethod awaken-cell ((c cell))
  (assert (c-inputp c))
  ;
  ; nothing to calculate, but every cellular slot should be output
  ;
  (when (> *data-pulse-id* (c-pulse-observed c))
    ;(trc nil "awaken-pulsing" :*dpid* *data-pulse-id* :cdpid (c-pulse-observed c) c)
    (setf (c-pulse-observed c) *data-pulse-id*)
    (trc nil "awaken cell observing" c *data-pulse-id*)
    (let ((*observe-why* :awaken-cell))
      (slot-value-observe (c-slot-name c) (c-model c) (c-value c) nil nil c))
    (ephemeral-reset c)))

(defmethod awaken-cell ((c c-ruled))
  (let (*depender*)
    (calculate-and-set c :fn-awaken-cell nil)))

#+cormanlisp ; satisfy CormanCL bug
(defmethod awaken-cell ((c c-dependent))
  (let (*depender*)
    (trc nil "awaken-cell c-dependent clearing *depender*" c)
    (calculate-and-set c :fn-awaken-cell nil)))

(defmethod awaken-cell ((c c-drifter))
  ;
  ; drifters *begin* valid, so the derived version's test for unbounditude
  ; would keep (drift) rule ever from being evaluated. correct solution
  ; (for another day) is to separate awakening (ie, linking to independent
  ; cs) from evaluation, tho also evaluating if necessary during
  ; awakening, because awakening's other role is to get an instance up to speed
  ; at once upon instantiation 
  ;
  (calculate-and-set c :fn-awaken-cell nil)
  (cond ((c-validp c) (c-value c))
        ((c-unboundp c) nil)
        (t "illegal state!!!")))
