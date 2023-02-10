;; -*- mode: Lisp; Syntax: Common-Lisp; Package: triple-cells; -*-
;;;
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;


(in-package :3c)

;; --- ag utils -----------------------

#+test
(progn
  (make-tutorial-store)
  (let ((s (mk-upi "a"))
        (p (new-blank-node)))
    (loop repeat 10
          do (add-triple s (mk-upi (random 10)) p))
    (index-new-triples)
    (loop for tr in (get-triples-list :s s)
        do (print (upi->value (predicate tr))))))

(defun triple-value (tr)
  (when tr
    (upi->value (object tr))))

(export! get-sp get-po triple-value get-spo mk-upi get-sp-value ensure-triple
  get-po-subject get-sps tr-dump tr-zap ordered-objects db-delete-node)

(defun get-sp (s p)
  (get-triple :s s :p p))

(defun get-sps (s p)
  (get-triples-list :s s :p p))

(defun get-po (p o)
   (get-triple :p p :o o))

(defun get-po-subject (p o)
  (b-when tr (get-triple :p p :o o)
    (subject tr)))

(defun get-spo (s p o)
  (get-triple :s s :p p :o o))

(defun get-sp-value (s p)
  (triple-value (get-sp s p)))

(defun get-sp-keyword (s p)
  (intern (get-sp-value s p) :keyword))

(export! get-sp-keyword)

(defun db-delete-node (s)
  (delete-triples :s s)
  (delete-triples :o s))

(defun mk-upi (v)
  (typecase v
    (string (literal v))
    (symbol (mk-upi (symbol-name v)))
    (integer (value->upi v :long))
    (future-part v)
    (otherwise (if (upip v) v
                 (error "not upi-able ~a ~a" (type-of v) v)))))

(defun ordered-objects (trs order-by)
  (mapcar 'cdr
    (sort (loop for tr in trs
              for obn = (object tr)
              for n = (get-sp-value obn order-by)
              for x upfrom 1 ;; use for robustness
              collect (cons (or n x) obn))
      '< :key 'car)))

(defun ensure-triple (s p o)
  (unless (get-spo s p o)
    (add-triple s p o)))

(defun tr-dump (tag tr &optional (depth 0) (seen (make-hash-table)))
  (unless (gethash (triple-id tr) seen)
    (setf (gethash (triple-id tr) seen) depth)
    (trc "tr> " tag depth tr)
    (when (blank-node-p (object tr))
      (loop for tr in (get-triples-list :s (object tr))
            do (tr-dump tag tr (1+ depth) seen)))))

(defun tr-zap (tag tr &optional (depth 0) (seen (make-hash-table)))
  (unless (gethash (triple-id tr) seen)
    (setf (gethash (triple-id tr) seen) depth)
    (trc "tag" depth tr)
    (when (blank-node-p (object tr))
      (loop for tr in (get-triples-list :s (object tr))
          do (tr-zap tag tr (1+ depth) seen)))
    (delete-triple (triple-id tr))))