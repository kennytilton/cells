;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
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

(defmodel ring-node ()
  ((router-ids :cell nil :initform nil :initarg :router-ids :accessor router-ids)
   (system-status :initform (c-in 'up) :initarg :system-status :accessor system-status
     :documentation "'up, 'down, or 'unknown if unreachable")
   (reachable :initarg :reachable :accessor reachable
      :initform (c? (not (null ;; convert to boolean for readable test output
                          (find self (^reachable-nodes .parent))))))))

(defun up (self) (eq 'up (^system-status)))

(defmodel ring-net (family)
  (
   (ring :cell nil :initform nil :accessor ring :initarg :ring)
   (sys-node :cell nil :initform nil :accessor sys-node :initarg :sys-node)
   (reachable-nodes :initarg :reachable-nodes :accessor reachable-nodes
      :initform (c? (contiguous-nodes-up
                     (find (sys-node self) (^kids)
                       :key 'md-name))))
   )
  (:default-initargs
      :kids (c? (assert (sys-node self))
              (assert (find (sys-node self) (ring self)))
              (loop with ring = (ring self)
                  for triples on (cons (last1 ring)
                                   (append ring (list (first ring))))
                  when (third triples)
                  collect (destructuring-bind (ccw node cw &rest others) triples
                            (declare (ignorable others))
                            (print (list ccw node cw))
                            (make-instance 'ring-node
                              :md-name node
                              :router-ids (list ccw cw)))))))

(defun contiguous-nodes-up (node &optional (visited-nodes (list)))
  (assert (not (find (md-name node) visited-nodes)))

  (if (not (up node))
      (values nil (push (md-name node) visited-nodes))
    (progn
      (push (md-name node) visited-nodes)
      (values 
       (list* node
         (mapcan (lambda (router-id)
                   (unless (find router-id visited-nodes)
                     (multiple-value-bind (ups new-visiteds)
                         (contiguous-nodes-up (fm-other! node router-id) visited-nodes)
                       (setf visited-nodes new-visiteds)
                       ups)))
           (router-ids node)))
       visited-nodes))))

(defun test-ring-net ()
  (flet ((dump-net (net msg)
           (print '----------------------)
           (print `(*** dump-net ,msg ******))
           (dolist (n (kids net))
             (print (list n (system-status n)(reachable n)(router-ids n))))))
    (cell-reset)
    (let ((net (make-instance 'ring-net
                 :sys-node 'two
                 :ring '(one two three four five six))))
      (dump-net net "initially")
      (setf (system-status (fm-other! net 'three)) 'down)
      (dump-net net "down goes three!!")
      (setf (system-status (fm-other! net 'six)) 'down)
      (dump-net net "down goes six!!!"))))

#+do-it
(test-ring-net)
               