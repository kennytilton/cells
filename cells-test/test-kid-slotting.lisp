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

(defmd image (family) left top width height)

(defun i-right (x) (+ (left x) (width x)))
(defun i-bottom (x) (+ (top x) (height x)))

(defmd stack (image)
  justify
  (.kid-slots :initform (lambda (self)
                          (declare (ignore self))
                          (list
                           (mk-kid-slot (left :if-missing t)
                             (c? (+ (left .parent)
                                   (ecase (justify .parent)
                                     (:left 0)
                                     (:center (floor (- (width .parent) (^width)) 2))
                                     (:right (- (width .parent) (^width)))))))
                           (mk-kid-slot (top)
                             (c? (bif (psib (psib))
                                   (i-bottom psib)
                                   (top .parent))))))
    :accessor kid-slots
    :initarg :kid-slots))
;;
;; kid-slotting exists largely so graphical containers can be defined which arrange their
;; component parts without those parts' cooperation. so a stack class can be defined as shown
;; and then arbitrary components thrown in as children and they will be, say, right-justified
;; because they will be endowed with rules as necessary to achieve that end by the parent stack.
;;
;; note the ifmissing option, which defaults to nil. the stack's goal is mainly to manage the
;; top attribute of each kid to match any predecessor's i-bottom attribute. the stack will as a
;; a convenience arrange for horizontal justification, but if some kid chose to define its
;; left attribute that would be honored.
;;
(def-cell-test cv-kid-slotting ()
  (cells-reset)
  (let ((stack (make-instance 'stack
                          :left 10 :top 20
                        :width 500 :height 1000
                        :justify (c-in :left)
                        :kids (c? (eko ("kids") (loop for kn from 1 to 4
                                    collect (make-kid 'image
                                              :top 0 ;; overridden
                                              :width (* kn 10)
                                              :height (* kn 50)))))
                        )))
    (ct-assert (eql (length (kids stack)) 4))
    (ct-assert (and (eql 10 (left stack))
                    (every (lambda (k) (eql 10 (left k)))
                           (kids stack))))
    (ct-assert (every (lambda (k)
                        (eql (top k) (i-bottom (fm-prior-sib k))))
                      (cdr (kids stack))))

    (setf (justify stack) :right)
    (ct-assert (and (eql 510 (i-right stack))
                    (every (lambda (k) (eql 510 (i-right k)))
                           (kids stack))))
    ))
