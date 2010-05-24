;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cells)

(defun c-variable-accessor (symbol)
  (assert (symbolp symbol))
  (c-variable-reader symbol))

(defun (setf c-variable-accessor) (value symbol)
  (assert (symbolp symbol))
  (c-variable-writer value symbol))

(defun c-variable-reader (symbol)
  (assert (symbolp symbol))
  (assert (get symbol 'cell))
  (cell-read (get symbol 'cell)))

(defun c-variable-writer (value symbol)
  (assert (symbolp symbol))
  (setf (md-slot-value nil symbol) value)
  (setf (symbol-value symbol) value))

(export! def-c-variable)

(defmacro def-c-variable (v-name cell &key ephemeral owning unchanged-if)
  (declare (ignore unchanged-if))
  (let ((c 'whathef)) ;;(gensym)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (define-symbol-macro ,v-name (c-variable-accessor ',v-name))
         (setf (md-slot-cell-type 'null ',v-name) (when ,ephemeral :ephemeral))
         (when ,owning
           (setf (md-slot-owning 'null ',v-name) t)))
       (eval-when (:load-toplevel)
         (let ((,c ,cell))
           (md-install-cell nil ',v-name ,c)
           (awaken-cell ,c)))
       ',v-name)))


(defobserver *kenny* ()
  (trcx kenny-obs new-value old-value old-value-boundp))

#+test
(def-c-variable *kenny* (c-in nil))


#+test
(defmd kenny-watcher ()
  (twice (c? (bwhen (k *kenny*)
               (* 2 k)))))

(defobserver twice ()
  (trc "twice kenny is:" new-value self old-value old-value-boundp))

#+test-ephem
(progn
  (cells-reset)
  (let ((tvw (make-instance 'kenny-watcher)))
    (trcx twice-read (twice tvw))
    (setf *c-debug* nil)
    (setf *kenny* 42)
    (setf *kenny* 42)
    (trcx post-setf-kenny *kenny*)
    (trcx print-twice (twice tvw))
    ))

#+test
(let ((*kenny* 13)) (print *kenny*))
     
#+test
(let ((c (c-in 42)))
  (md-install-cell '*test-c-variable* '*test-c-variable* c)
  (awaken-cell c)
  (let ((tvw (make-instance 'test-var-watcher)))
    (trcx twice-read (twice tvw))
    (setf *test-c-variable* 69)
    (trcx print-testvar *test-c-variable*)
    (trcx print-twice (twice tvw))
    (unless (eql (twice tvw) 138)
      (inspect (md-slot-cell tvw 'twice))
      (inspect c)
      ))
  )

#+test2
(let ((tvw (make-instance 'test-var-watcher :twice (c-in 42))))
  (let ((c (c? (trcx joggggggggging!!!!!!!!!!!!!!!)
             (floor (twice tvw) 2))))
    (md-install-cell '*test-c-variable* '*test-c-variable* c)
    (awaken-cell c)
    (trcx print-testvar *test-c-variable*)
    (trcx twice-read (twice tvw))
    (setf (twice tvw) 138)
    (trcx print-twice (twice tvw))
    (trcx print-testvar *test-c-variable*)
    (unless (eql *test-c-variable* 69)
      (inspect (md-slot-cell tvw 'twice))
      (inspect c)
      ))
  )

