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

(defmodel counter-10 ()
  ((ct :initarg :ct :initform nil :accessor ct)
   (ct10 :initarg :ct10 :initform nil
     :accessor ct10)))

(defun cv-test-f-sensitivity ()
  (cell-reset)
  (with-metrics (t nil "cv-test-f-sensitivity")
    (let ((self (make-be 'counter-10
                  :ct (c-in 0)
                  :ct10 (c? (count-it :ct10-rule)
                          (f-sensitivity :dummy-id (10)
                            (^ct))))))
      (cv-assert (zerop (^ct10)))
      (loop for n below 30
          do (cv-assert (eq (^ct10) (* 10 (floor (^ct) 10))))
            (incf (ct self))))
    (cv-assert (eql 4 (count-of :ct10-rule)))))

(defun cv-test-f-delta ()
  (cell-reset)
  (with-metrics (t nil "cv-test-f-delta")
    (let ((self (make-be 'counter-10
                  :ct (c-in 0)
                  :ct10 (c? (count-it :ct10-rule)
                          (trc "runnning ct10-rule 1")
                          (f-delta :dummy ()
                            (^ct))))))
      (cv-assert (zerop (^ct10)))
      (cv-assert (zerop (^ct)))
      (loop for n below 4
          do (trc "loop incf ct" n)
            (incf (ct self) n)
            (cv-assert (eql (^ct10) n))))
    (cv-assert (eql 4 (count-of :ct10-rule))))

  (with-metrics (t nil "cv-test-f-delta-sensitivity")
    (let ((self (make-be 'counter-10
                  :ct (c-in 0)
                  :ct10 (c? (count-it :ct10-rule)
                          (f-delta :xxx (:sensitivity 4)
                            (^ct))))))
      (cv-assert (null (^ct10)))
      (cv-assert (zerop (^ct)))
      (loop for n below 4
          do (trc "loop incf ct" n)
            (incf (ct self) n)
            (ecase n
              ((0 1 2) (cv-assert (null (^ct10))))
              (3 (cv-assert (eql (^ct10) 6)))
              (4 (cv-assert (eql (^ct10) 4)))))
      (cv-assert (eql 2 (count-of :ct10-rule))))))

