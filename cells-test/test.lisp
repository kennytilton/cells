;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
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

#| Synapse Cell Unification Notes

- start by making Cells synapse-y

- make sure outputs show right old and new values
- make sure outputs fire when they should

- wow: test the Cells II dictates: no output callback sees stale data, no rule
sees stale data, etc etc

- test a lot of different synapses

- make sure they fire when they should, and do not when they should not

- make sure they survive an evaluation by the caller which does not branch to
them (ie, does not access them)

- make sure they optimize away

- test with forms which access multiple other cells

- look at direct alteration of a caller

- does SETF honor not propagating, as well as a c-ruled after re-calcing

- do diff unchanged tests such as string-equal work

|#

#| do list


-- test drifters (and can they be handled without creating a special
subclass for them?)

|#

;;;(eval-when (compile load)
;;;  (proclaim '(optimize (speed 2) (safety 3) (space 1) (debug 3))))

(in-package :cells)

(defvar *cell-tests* nil)

#+go
(test-cells)


(defun test-cells ()
  (loop for test in (reverse *cell-tests*)
        when t ; (eq 'cv-test-person-5 test)
        do (cell-test-init test)
        (funcall test))
  (print (make-string 40 :initial-element #\*))
  (print (make-string 40 :initial-element #\*))
  (print "*** Cells-test successfully completed **")
  (print (make-string 40 :initial-element #\*))
  (print (make-string 40 :initial-element #\*)))

(defun cell-test-init (name)
  (print (make-string 40 :initial-element #\!))
  (print `(starting test ,name))
  (print (make-string 40 :initial-element #\!))
  (cells-reset))

(defmacro def-cell-test (name &rest body)
  `(progn
     (pushnew ',name *cell-tests*)
     (defun ,name ()
       (cells-reset)
       ,@body)))

(defmacro ct-assert (form &rest stuff)
  `(progn
     (print `(attempting ,',form))
    (assert ,form () "Error with ~a >> ~a" ',form (list ,@stuff))))

;; test huge number of useds by one rule

(defmd m-index (family)
  :value (c? (bwhen (ks (^kids))
                  ;(trc "chya" (mapcar 'value ks))
                  (apply '+ (mapcar 'value ks)))))

(def-cell-test many-useds
    (let ((i (make-instance 'm-index)))
      (loop for n below 100
          do (push (make-instance 'model
                     :fm-parent i
                     :value (c-in n))
               (kids i)))
      (trc "index total" (value i))
      (ct-assert (= 4950 (value i)))))

#+test
(many-useds)

(defmd m-null () 
  (aa :cell nil :initform nil :initarg :aa :accessor aa))


(def-cell-test m-null
    (let ((m (make-instance 'm-null :aa 42)))
      (ct-assert (= 42 (aa m)))
      (ct-assert (= 21 (let ((slot 'aa))
                         (funcall (fdefinition `(setf ,slot)) (- (aa m) 21) m))))
      :okay-m-null))

(defmd m-solo () m-solo-a m-solo-b)

(def-cell-test m-solo
    (let ((m (make-instance 'm-solo
               :m-solo-a (c-in 42)
               :m-solo-b (c? (trc "b fires")(* 2 (^m-solo-a))))))
      (ct-assert (= 42 (m-solo-a m)))
      (ct-assert (= 84 (m-solo-b m)))
      (decf (m-solo-a m))
      (ct-assert (= 41 (m-solo-a m)))
      (ct-assert (= 82 (m-solo-b m)))
      :okay-m-null))

(defmd m-var () m-var-a m-var-b)

(defobserver m-var-b ()
  (print `(output m-var-b ,self ,new-value ,old-value)))

(def-cell-test m-var
  (let ((m (make-instance 'm-var :m-var-a (c-in 42) :m-var-b 1951)))
    (ct-assert (= 42 (m-var-a m)))
    (ct-assert (= 21 (decf (m-var-a m) 21)))
    (ct-assert (= 21 (m-var-a m)))
    :okay-m-var))

(defmd m-var-output ()
  cbb
  (aa :cell nil :initform nil :initarg :aa :accessor aa))

(defobserver cbb ()
  (trc "output cbb" self)
  (setf (aa self) (- new-value (if old-value-boundp
                                   old-value 0))))

(def-cell-test m-var-output
  (let ((m (make-instance 'm-var-output :cbb (c-in 42))))
    (ct-assert (eql 42 (cbb m)))
    (ct-assert (eql 42 (aa m)))
    (ct-assert (eql 27 (decf (cbb m) 15)))
    (ct-assert (eql 27 (cbb m)))
    (ct-assert (eql -15 (aa m)))
    (list :okay-m-var (aa m))))

(defmd m-var-linearize-setf () ccc ddd)

(defobserver ccc ()
  (with-integrity (:change)
    (setf (ddd self) (- new-value (if old-value-boundp
                                      old-value 0)))))

(def-cell-test m-var-linearize-setf
  (let ((m (make-instance 'm-var-linearize-setf
                    :ccc (c-in 42)
                    :ddd (c-in 1951))))
    
    (ct-assert (= 42 (ccc m)))
    (ct-assert (= 42 (ddd m)))
    (ct-assert (= 27 (decf (ccc m) 15)))
    (ct-assert (= 27 (ccc m)))
    (ct-assert (= -15 (ddd m)))
    :okay-m-var))

;;; -------------------------------------------------------

(defmd m-ruled ()
  eee
  (fff (c? (floor (^ccc) 2))))

(defobserver eee ()
  (print `(output> eee ,new-value old ,old-value)))

(defobserver fff ()
  (print `(output> eee ,new-value old ,old-value)))

(def-cell-test m-ruled
  (let ((m (make-instance 'm-ruled
                    :eee (c-in 42)
                    :fff (c? (floor (^eee) 2)))))
    (trc "___Initial TOBE done____________________")
    (print `(pulse ,*data-pulse-id*))
    (ct-assert (= 42 (eee m)))
    (ct-assert (= 21 (fff m)))
    (ct-assert (= 36 (decf (eee m) 6)))
    (print `(pulse ,*data-pulse-id*))
    (ct-assert (= 36 (eee m)))
    (ct-assert (= 18 (fff m)) m)
    :okay-m-ruled))

(defmd m-worst-case ()
  (wc-x (c-input () 2))
  (wc-a (c? (prog2
              (trc "Start A")
                (when (oddp (wc-x self))
                  (wc-c self))
              (trc "Stop A"))))
  (wc-c (c? (evenp (wc-x self))))
  (wc-h (c? (or (wc-c self)(wc-a self)))))

(defun dependency-dump (self)
  (let ((slot-cells (loop for esd in (class-slots (class-of self))
                   for sn = (slot-definition-name esd)
                   for c = (md-slot-cell self sn)
                   when c
                     collect (cons sn c))))
    (trc "dependencies of" self)
    (loop for (sn . c) in slot-cells
          do (trc "slot" sn :callers (mapcar 'c-slot-name (c-callers c))))))

(def-cell-test m-worst-case
  (let ((m (make-instance 'm-worst-case)))
    (dependency-dump m)
    (trc "___Initial TOBE done____________________")
    (ct-assert (eql t (wc-c m)))
    (ct-assert (eql nil (wc-a m)))
    (ct-assert (eql t (wc-h m)))
    (dependency-dump m)
    (ct-assert (eql 3 (incf (wc-x m))))))

(defmd c?n-class ()
  aaa bbb
  (sum (c? (+ (^aaa) (^bbb)))))

(def-cell-test test-c?n ()
  (let ((self (make-instance 'c?n-class
                :aaa (c?n (+ (^bbb) 2))
                :bbb (c-in 40))))
    (ct-assert (= (^bbb) 40)) ;; make sure I have not broken (setf slot-value)...it happens
    (ct-assert (= (^aaa) 42)) ;; make sure the rule ran and the value stored as the slot value
    (ct-assert (= (^sum) 82)) ;; make sure a normal rule works off the others
    (setf (^bbb) 100)
    (ct-assert (= (^bbb) 100)) ;; just checking
    (ct-assert (= (^aaa) 42))  ;; make sure the rule did not run again
    (ct-assert (= (^sum) 142)) ;; ... but the other rule does fire
    (setf (^aaa) -58)
    (ct-assert (= (^aaa) -58)) ;; ... we can setf the once-ruled slot
    (ct-assert (= (^sum) 42))  ;; ... propagation still works from the once-ruled, now-input slot
    ))
