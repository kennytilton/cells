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



(defmodel m-cyc ()
  ((m-cyc-a :initform (c-in nil) :initarg :m-cyc-a :accessor m-cyc-a)
   (m-cyc-b :initform (c-in nil) :initarg :m-cyc-b :accessor m-cyc-b)))

(def-c-output m-cyc-a ()
  (print `(output m-cyc-a ,self ,new-value ,old-value))
  (setf (m-cyc-b self) new-value))

(def-c-output m-cyc-b ()
  (print `(output m-cyc-b ,self ,new-value ,old-value))
  (setf (m-cyc-a self) new-value))

(defun m-cyc () ;;def-cell-test m-cyc
    (let ((m (make-be 'm-cyc)))
      (print `(start ,(m-cyc-a m)))
      (setf (m-cyc-a m) 42)
      (assert (= (m-cyc-a m) 42))
      (assert (= (m-cyc-b m) 42))))

#+(or)
(m-cyc)

(defmodel m-cyc2 ()
  ((m-cyc2-a :initform (c-in 0) :initarg :m-cyc2-a :accessor m-cyc2-a)
   (m-cyc2-b :initform (c? (1+ (^m-cyc2-a)))
     :initarg :m-cyc2-b :accessor m-cyc2-b)))

(def-c-output m-cyc2-a ()
  (print `(output m-cyc2-a ,self ,new-value ,old-value))
  #+(or) (when (< new-value 45)
    (setf (m-cyc2-b self) (1+ new-value))))

(def-c-output m-cyc2-b ()
  (print `(output m-cyc2-b ,self ,new-value ,old-value))
  (when (< new-value 45)
    (setf (m-cyc2-a self) (1+ new-value))))

(def-cell-test m-cyc2
    (cell-reset)
    (let ((m (make-be 'm-cyc2)))
      (print '(start))
      (setf (m-cyc2-a m) 42)
      (describe m)
      (assert (= (m-cyc2-a m) 44))
      (assert (= (m-cyc2-b m) 45))
      ))

#+(or)
(m-cyc2)


