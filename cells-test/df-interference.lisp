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

(defvar *eex* 0)

(defmodel xx3 ()
  ((aa :initform (c-in 0) :initarg :aa :accessor aa)
   (dd :initform (c? (min 0 (+ (^cc) (^bb)))) :initarg :dd :accessor dd)
   (ddx :initform (c? (+ (^cc) (^bb))) :initarg :ddx :accessor ddx)
   (cc :initform (c? (+ (^aa) (^bb))) :initarg :cc :reader cc)
   (bb :initform (c? (* 2 (^aa))) :initarg :bb :accessor bb)
   (ee :initform (c? (+ (^aa) (^dd))) :initarg :ee :reader ee)
   (eex :initform (c?  (trc "in rule of eex, *eex* now" *eex*)
                    (+ (^aa) (^ddx))) :initarg :eex :reader eex)
   ))

(defobserver aa ((self xx3))
    (trc nil "output aa:" new-value))

(defobserver bb ((self xx3))
   (trc nil "output bb:" new-value))

(defobserver cc ((self xx3))
    (trc nil "output cc:" new-value))

(defobserver dd ((self xx3))
    (trc nil "output dd:" new-value))

(defobserver ee ((self xx3))
   (trc nil "output ee:" new-value))

(defobserver eex ((self xx3))
  (incf *eex*)
    (trc "output eex:" new-value *eex*))

;;
;; here we look at just one problem, what i call dataflow interference. consider
;; a dependency graph underlying:
;;
;;     - a depends on b and c, and...
;;     - b depends on c
;;
;; if c changes, depending on the accident of the order in which a and b happened to
;; be first evaluated, a might appear before b on c's list of dependents (callers). then the
;; following happens:
;;
;;     - c triggers a
;;     - a calculates off the new value of c and an obsolete cached value for b
;;     - a outputs an invalid value and triggers any dependents, all of whom recalculate
;;         using a's invalid value
;;     - c triggers b
;;     - b recalculates and then triggers a, which then recalculates correctly and outputs and triggers
;;         the rest of the df graph back into line
;;
;; the really bad news is that outputs go outside the model: what if the invalid output caused
;; a missile launch? sure, a subsequent correct calculation comes along shortly, but 
;; irrevocable damage may have been done.
;;

(def-cell-test df-test ()
  (cells-reset)
  (let* ((*eex* 0)
         (it (make-instance 'xx3)))
    (trc "eex =" *eex*)
    (ct-assert (eql *eex* 1))
    ;;(inspect it);;(cellbrk)
    (ct-assert (and (eql (aa it) 0)(eql (bb it) 0)(eql (cc it) 0)))
    (ct-assert (and (eql (dd it) 0)(eql (ddx it) 0)(eql (ee it) 0)(eql (eex it) 0)))
    
    ;;;- interference handling
    ;;;
    (let ((*eex* 0))
      (trc "--------- 1 => (aa it) --------------------------")
      (setf (aa it) 1)
      
      (ct-assert (and (eql (aa it) 1)(eql (bb it) 2)(eql (cc it) 3)))
      (trc "dd,ddx:" (dd it) (ddx it) )
      (ct-assert (and (eql (dd it) 0)(eql (ddx it) 5)))
      (ct-assert (and (eql (ee it) 1)(eql (eex it) 6)))
      (ct-assert (eql *eex* 1)))
    
    (let ((*eex* 0))
      (trc "--------- 2 => (aa it) --------------------------")
      (setf (aa it) 2)
      (ct-assert (and (eql (aa it) 2)(eql (bb it) 4)(eql (cc it) 6)
                   (eql (dd it) 0)(eql (ddx it) 10)(eql (ee it) 2)(eql (eex it) 12)))
      (ct-assert (eql *eex* 1)))
    
    (dolist (c (cells it))
      (trc "cell is" c)
      (when (typep (cdr c) 'cell)
        (print `(notifier ,c))
        (dolist (u (c-callers (cdr c)))
          (print `(___ ,u)))))
    ))


