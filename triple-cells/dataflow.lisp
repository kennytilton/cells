;; -*- mode: Lisp; Syntax: Common-Lisp; Package: triple-cells; -*-
;;;
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;

(in-package :3c)

(defun 3c-propagate (cell)
  (3c-ufb-add !ccc:ufb-tell-dependents cell)
  (trc "3c-propagate of" cell (gethash cell *3c-dyno-proxy*))
  (maphash (lambda (k v)
             (trc "dyno dep" k v)) *3c-dyno-proxy*)
  (bwhen (dd (gethash cell *3c-dyno-proxy*))
    (trc "bingo tell dyno ~a ~a ~a" cell dd (cells::c-caller-store dd))
    (cells::c-propagate-to-callers dd)))

;;; --- integrity -----------------(part-value prior-value)-----------------------------

(defun 3c-ensure-current (cell &optional s p) ;; when we don't have s/p extend to work backwards from cell
  (unless s
    (setf s (cell-model cell))
    (setf p (cell-predicate cell)))
  ;(trc "3c-ensure-current" s p)
  (when (and cell (3c-ruled? cell))
    (unless (upi= (3c-pulse) (3c-cell-pulse cell))
     ; (trc "old" (3c-cell-value cell))
      ;(trc "HEY!!! what happened to checking if necessary to rerun rule?!")
      (let* ((prior-value (3c-cell-value cell))
            (new-value (progn
                         (clear-usage cell)
                         (funcall (3c?-rule cell) cell
                           prior-value
                           t)))
            (test (or (bwhen (test (get-sp-value cell !ccc:test))
                        (intern test))
                    'EQL)))
        
        (if (funcall test new-value prior-value)
            (3c-cell-make-current cell)
          (progn
            ;(trc "prop new" new-value :prior prior-value)
            (let ((prior-value (3c-cell-value cell)))
              (setf (3c-cell-value cell) new-value)
              (delete-triples :s s :p p)
              (when new-value
                (add-triple s p (mk-upi new-value)))
              (3c-propagate cell)
              (cell-observe-change cell s p new-value prior-value t)
              (when (3c-ephemeral? cell)
                (add-triple !ccc:ufb-reset-ephemerals (mk-upi 42) cell)))))))))
