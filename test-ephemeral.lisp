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


(defmodel m-ephem ()
  ((m-ephem-a :cell :ephemeral :initform nil :initarg :m-ephem-a :accessor m-ephem-a)
   (m-test-a :cell nil :initform nil :initarg :m-test-a :accessor m-test-a)
   (m-ephem-b :cell :ephemeral :initform nil :initarg :m-ephem-b :accessor m-ephem-b)
   (m-test-b :cell nil :initform nil :initarg :m-test-b :accessor m-test-b)))

(def-c-output m-ephem-a ()
  (setf (m-test-a self) new-value))

(def-c-output m-ephem-b ()
  (setf (m-test-b self) new-value))

(def-cell-test m-ephem
    (let ((m (make-be 'm-ephem :m-ephem-a (c-in nil) :m-ephem-b (c? (* 2 (or (^m-ephem-a) 0))))))
      (ct-assert (null (slot-value m 'm-ephem-a)))
      (ct-assert (null (m-ephem-a m)))
      (ct-assert (null (m-test-a m)))
      (ct-assert (null (slot-value m 'm-ephem-b)))
      (ct-assert (null (m-ephem-b m)))
      (ct-assert (zerop (m-test-b m)))
      (setf (m-ephem-a m) 3)
      (ct-assert (null (slot-value m 'm-ephem-a)))
      (ct-assert (null (m-ephem-a m)))
      (ct-assert (eql 3 (m-test-a m)))
      ;
      (ct-assert (null (slot-value m 'm-ephem-b)))
      (ct-assert (null (m-ephem-b m)))
      (ct-assert (eql 6 (m-test-b m)))
      ))



