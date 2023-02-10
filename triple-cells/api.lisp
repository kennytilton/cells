;; -*- mode: Lisp; Syntax: Common-Lisp; Package: triple-cells; -*-
;;;
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;


(in-package :3c)

;;; --- API ---------------------------------------

(defun 3c-init ()
  (setf *calc-nodes* nil)
  (setf *3c?* (make-hash-table :test 'equal))
  (setf *3c-dyno-proxy* (make-hash-table :test 'equalp))
  (setf *3c-observers* (make-hash-table :test 'equalp)))

;;; --- API constructors -------------------------------

(defun 3c-in (initial-value &key ephemeral test observer &aux (c (new-blank-node)))
  (add-triple c !ccc:type !ccc:input)
  (when observer
    (add-triple c !ccc:observer-is (mk-upi observer)))
  (setf (3c-cell-value c) initial-value)
  (when ephemeral
    (add-triple c !ccc:ephemeral !ccc:t))
  (when test
    (add-triple c !ccc:test (mk-upi test)))
  c)

(defmacro 3c? (rule &key test ephemeral observer)
  `(call-3c? '(lambda (node cache cache?)
                (declare (ignorable cache cache?))
                (let ((*calc-nodes* (cons node *calc-nodes*)))
                  ,rule))
     :test ,test
     :observer ,observer
     :ephemeral ,ephemeral))

(defun call-3c? (rule &key test ephemeral observer)
  (let* ((c (new-blank-node)))
    (add-triple c !ccc:type !ccc:ruled)
    (add-triple c !ccc:rule (mk-upi (prin1-to-string rule)))
    (when ephemeral
      ;(trc "bingo ephemeral" rule)
      (add-triple c !ccc:ephemeral !ccc:t))
    (when test
      (add-triple c !ccc:test (mk-upi test)))
    (when observer
      (add-triple c !ccc:observer-is (mk-upi observer)))
    (let ((rule-fn (eval rule)))
      ;(trc "rule-fn" rule-fn :from rule)
      (setf (3c?-rule c) rule-fn)
      ;(trc "c? type tr" tr-c)
      ;(trc "c? value tr" tr-cv)
      c)))

;;; --- API accessors

(defstruct (3cell-dyno (:include cells::cell)))

;; work out shift to triple-cells when necessary....
;;;(defmethod cells::ensure-value-is-current ((c 3cell-dyno) debug-id ensurer)
;;;  (declare (ignorable debug-id ensurer))
;;;  (3c-ensure-current (cells::c-model c))
;;;  (3c-cell-value (cells::c-model c)))

(export! 3c 3c-find-id)

(defun 3c (s p)
  (assert (and s p))
  (bif (cell (stmt-cell s p))
    (progn
      (3c-ensure-current cell s p)
      (when cells::*depender*
        (print (list "3c > dyno dpender" cells::*depender* :sees-3cell cell (type-of cell)))
        (let ((proxy (or (gethash cell *3c-dyno-proxy*)
                       (setf (gethash cell *3c-dyno-proxy*)
                         (make-3cell-dyno
                          :model cell)))))
          (cells::record-caller proxy)))
      (when *calc-nodes*
        (assert (listp *calc-nodes*))
        (assert (not (find cell *calc-nodes*))() "Circularity? ~a ~a" cell *calc-nodes*)
        (ensure-triple (car *calc-nodes*) !ccc:uses cell))
        
      (get-sp-value s p))
    (get-sp-value s p)))

(defun (setf 3c) (new-value s p)
  (let* ((cell (stmt-cell s p))
         (tr-value (get-sp s p))
         (prior-value (when tr-value (upi->value (object tr-value)))))
      
    (assert cell () "subject ~a pred ~a not mediated by input cell so cannot be changed from ~a to ~a"
        s p prior-value new-value)
      ;(trc "tr-cell" (triple-id tr-cell))
      ;(trc "tr-value" (triple-id tr-value))

      (unless (equal new-value prior-value)
        (with-3c-integrity (:change cell)
          (when tr-value
            (delete-triple (triple-id tr-value)))

          (let ((new-value-upi (mk-upi new-value)))
            (add-triple s p new-value-upi)
            ; cell maintenance, including its own copy of value
            (delete-triples :s cell :p !ccc:value)
            (add-triple cell !ccc:value new-value-upi)
            (3c-propagate cell)
            (cell-observe-change cell s p new-value prior-value t)
            (when (3c-ephemeral? cell)
              (add-triple !ccc:ufb-reset-ephemerals (mk-upi 42) cell)))))))


