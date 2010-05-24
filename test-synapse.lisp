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


(defmodel m-syn ()
  ((m-syn-a :initform nil :initarg :m-syn-a :accessor m-syn-a)
   (m-syn-b :initform nil :initarg :m-syn-b :accessor m-syn-b)
   (m-syn-factor :initform nil :initarg :m-syn-factor :accessor m-syn-factor)
   (m-sens :initform nil :initarg :m-sens :accessor m-sens)
   (m-plus :initform nil :initarg :m-plus :accessor m-plus)
   ))

(def-c-output m-syn-b ()
  (print `(output m-syn-b ,self ,new-value ,old-value)))



(def-cell-test m-syn
    (progn (cell-reset)
      (let* ((delta-ct 0)
             (sens-ct 0)
             (plus-ct 0)
             (m (make-be 'm-syn
                  :m-syn-a (c-in 0)
                  :m-syn-b (c? (incf delta-ct)
                             (trc nil "syn-b rule firing!!!!!!!!!!!!!!" delta-ct)
                             (eko (nil "syn-b rule returning")
                               (f-delta :syna-1 (:sensitivity 2)
                                 (^m-syn-a))))
                  :m-syn-factor (c-in 1)
                  :m-sens (c? (incf sens-ct)
                            (trc nil "m-sens rule firing ~d !!!!!!!!!!!!!!" sens-ct)
                            (* (^m-syn-factor)
                              (f-sensitivity :sensa (3) (^m-syn-a))))
                  :m-plus (c? (incf plus-ct)
                            (trc nil "m-plus rule firing!!!!!!!!!!!!!!" plus-ct)
                            (f-plusp :syna-2 (- 2 (^m-syn-a)))))))
        (ct-assert (= 1 delta-ct))
        (ct-assert (= 1 sens-ct))
        (ct-assert (= 1 plus-ct))
        (ct-assert (= 0 (m-sens m)))
        (trc "make-be complete. about to incf m-syn-a")
        (incf (m-syn-a m))
        (ct-assert (= 1 delta-ct))
        (ct-assert (= 1 sens-ct))
        (ct-assert (= 1 plus-ct))
        (ct-assert (= 0 (m-sens m)))
        (trc  "about to incf m-syn-a 2")
        (incf (m-syn-a m) 2)
        (trc nil "syn-b now" (m-syn-b m))
        (ct-assert (= 2 delta-ct))
        (ct-assert (= 2 sens-ct))
        (ct-assert (= 2 plus-ct))
        
        (ct-assert (= 3 (m-sens m)))
        (trc  "about to incf m-syn-a")
        (incf (m-syn-a m))
        (ct-assert (= 2 delta-ct))
        (ct-assert (= 2 sens-ct))
        (trc  "about to incf m-syn-factor")
        (incf (m-syn-factor m))
        (ct-assert (= 3 sens-ct))
        (ct-assert (= (m-sens m) (* (m-syn-factor m) (m-syn-a m))))
        (trc  "about to incf m-syn-a xxx")
        (incf (m-syn-a m))
        (ct-assert (= 2 delta-ct))
        (ct-assert (= 3 sens-ct))
        (trc  "about to incf m-syn-a yyyy")
        (incf (m-syn-a m))
        (ct-assert (= 3 delta-ct))
        (ct-assert (= 4 sens-ct))
        (ct-assert (= 2 plus-ct))
        (describe m)
        (print '(start)))))

(Def-c-output m-syn-a ()
  (trc "!!! M-SYN-A now =" new-value))

#+(or)
(m-syn)

