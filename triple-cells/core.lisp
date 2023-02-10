;; -*- mode: Lisp; Syntax: Common-Lisp; Package: triple-cells; -*-
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;

(in-package :3c)

;; --- triple-cells ---

(defvar *calc-nodes*)

(defun 3c-pulse-advance (&optional (dbg :anon-advance))
  (declare (ignorable dbg))
  ;(trc "PULSE> ---- advancing:" dbg)
  (delete-triples :s !ccc:integrity :p !ccc:pulse)
  (add-triple !ccc:integrity !ccc:pulse (new-blank-node)))

(defun 3c-pulse ()
  (bwhen (tr (get-triple :s !ccc:integrity :p !ccc:pulse))
    (object tr)))

;;; --- low-level 3cell accessors

(defun 3c-cell-value (c)
  (bwhen (tr (get-sp c !ccc:value))
    (part-value (object tr))))

(defun (setf 3c-cell-value) (new-value c)
  (3c-cell-make-current c)

  (delete-triples :s c :p !ccc:value)
  (when new-value
    (add-triple c !ccc:value (mk-upi new-value))))

(defun 3c-cell-make-current (c)
  (delete-triples :s c :p !ccc:pulse)
  (add-triple c !ccc:pulse (3c-pulse)))

(defun 3c-cell-pulse (c)
  (object (get-sp c !ccc:pulse)))

;;; --- rule storage -------------------------------

(defvar *3c?*)
(defvar *3c-dyno-proxy*)

#+dump
(maphash (lambda (k v) (trc "kk" k v)) *3c?*)

(defun (setf 3c?-rule) (rule c-node)
  (assert (functionp rule) () "3c?-rule setf not rule: ~a ~a" (type-of rule) rule)
  ;;(trc "storing rule!!!! for" c-node rule)
  (setf (gethash c-node *3c?*) rule))

(defun 3c?-rule (c-node)
  (or (gethash c-node *3c?*)
    (setf (gethash c-node *3c?*)
      (let ((rule$ (get-sp-value c-node !ccc:rule)))
        ;;(trc "got rule" rule$)
        (eval (read-from-string rule$))))))


;;; --- 3cell predicates -------------------------------------------

(defun 3c-cell? (c)
  (when (upip c)
    (get-sp c !ccc:type)))

(defun 3c-ephemeral? (c)
  (get-sp c !ccc:ephemeral))

(defun 3c-ruled? (c)
  (when (upip c)
    (bwhen (tr-type (get-sp c !ccc:type))
      (part= (object tr-type) !ccc:ruled))))

(defun 3c-input? (c)
  (when (upip c)
    (bwhen (tr-type (get-sp c !ccc:type))
      (part= (object tr-type) !ccc:input))))

;;; --- 3cell accessors -------------------------------------------

(defun 3c-class-of (s)
  (let ((type (object (get-sp s !ccc:instance-of))))
    (echo-sym (upi->value type))))

(defun 3c-predicate-of (p)
  (echo-sym (etypecase p
              (array (upi->value p))
              (future-part (part->string p)))))

(defun echo-sym (s)
  (intern (nsubstitute #\- #\#
            (up$ (string-trim "<>" s)))))

;;; --- access ------------------------------------------

(defun subject-cells-node (s)
  (bif (tr (get-triple :s s :p !ccc:cells))
    (object tr)
    (let ((n (new-blank-node)))
      (add-triple s !ccc:cells n)
      n)))

(defun (setf stmt-cell) (new-cell s p)
  (add-triple (subject-cells-node s) p new-cell))

(defun stmt-cell (s p)
  (bwhen (tr (get-sp (subject-cells-node s) p))
    (object tr)))

(defun cell-predicate (c)
  (object (get-sp c !ccc:is-cell-of-predicate)))

(defun cell-model (c)
  (object (get-sp c !ccc:is-cell-of-model)))

(defun 3c-install-cell (s p o)
  (add-triple (subject-cells-node s) p o)
  (add-triple o !ccc:is-cell-of-model s)
  (add-triple o !ccc:is-cell-of-predicate p))

(defun stmt-new (s p o &aux (tv o))
  (cond
   ((3c-cell? o)
    (3c-install-cell s p o)
    (cond
     ((3c-input? o)
      (bwhen (tv (3c-cell-value o))
        (add-triple s p (mk-upi tv)))
      (with-3c-integrity (!ccc:observe o)
        (cell-observe-change o s p tv nil nil)))
     ((3c-ruled? o)
      (with-3c-integrity (!ccc:awaken-ruled-cell o)
        (3c-awaken-ruled-cell o)))
     (t (break "unknown cell" o))))

  (t (when tv 
       (let ((tr (add-triple s p (mk-upi tv))))
         (trc "recording k under" p :id tr tv)
         (with-3c-integrity (!ccc:observe (mk-upi tr))
           (cell-observe-change o s p tv nil nil)))))))

(defun 3c-awaken-ruled-cell (c)
  (let ((s (cell-model c))
        (p (cell-predicate c))
        (tv (funcall (3c?-rule c) c nil nil)))
    ;(trc "awakening ruled" p)
    (setf (3c-cell-value c) tv)
    (cell-observe-change c s p tv nil nil)))

(defun 3c-make (type &key id)
  "Generates blank node and associates it with type and other options"
  (let ((node (new-blank-node)))
    (trc "3c-make storing type" type (type-of type))
    (add-triple node !ccc:instance-of type) 
    (when id
      (3c-register node id))
    node))

(defun 3c-register (node name)
  (add-triple (mk-upi name) !ccc:id node))

(defun 3c-find-id (name)
  (object (get-sp (mk-upi name) !ccc:id)))

(defun clear-usage (cell)
  (delete-triples :s cell :p !ccc:uses))

#+test
(progn
  (make-tutorial-store)
  (let ((x (3c-make !<plane> :id "x-plane")))
    (3c-find-id "x-plane")))
