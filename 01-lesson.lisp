(defpackage #:01-lesson (:use :cl :cells))

;;
;; We will keep making new packages so we can incrementally develop the
;; same class without newer versions stomping on earlier versions (by
;; being in the same package and effectively redefining earlier versions).
;;
(in-package #:01-lesson)

#|

(xxx :initarg :xxx :initform nil :accessor xxx)

|#

(defmodel rectangle ()
  ((len :initarg :len :accessor len
     :initform (c? (* 2 (width self))))
   (width :initarg :width :initform nil :accessor width))
  (:default-initargs
      :width (c? (/ (len self) 2))))

(cells::ct-assert (eql 21 (width (make-instance 'rectangle :len 42)))
