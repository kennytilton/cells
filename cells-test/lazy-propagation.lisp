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

(defvar *area*)
(defvar *density*)

(defmodel cirkl ()
  ((radius :initform (c-in 10) :initarg :radius :accessor radius)
   (area :initform (c?_ (incf *area*) (trc "in area rule it is now" *area*)
                     (* pi (^radius) (^radius))) :initarg :area :accessor area)
   (density :initform (c?_ (incf *density*)
                        (/ 1000 (^area))) :initarg :density :accessor density)))


#+(or)
(cv-laziness)

(def-cell-test cv-laziness ()
  (macrolet ((chk (area density)
               `(progn
                  (assert (= ,area *area*) () "area is ~a, should be ~a" *area* ,area)
                  (assert (= ,density *density*) () "density is ~a, should be ~a" *density* ,density)
                  (trc nil "cv-laziness ok with:" ,area ,density)))
             )
    (let ((*c-debug* t))
      (cells-reset)
    
      (let* ((*area* 0)
             (*density* 0)
             (it (make-instance 'cirkl)))
        (chk 0 0)

        (print `(area is ,(area it)))
        (chk 1 0)

        (setf (radius it) 1)
        (chk 1 0)

        (print `(area is now ,(area it)))
        (chk 2 0)
        (assert (= (area it) pi))

        (setf (radius it) 2)
        (print `(density is ,(density it)))
        (chk 3 1)
        
        (setf (radius it) 3)
        (chk 3 1)
        (print `(area is ,(area it)))
        (chk 4 1)
        it))))

#+(or)
(cv-laziness)

(defobserver area ()
  (trc "area is" new-value :was old-value))


