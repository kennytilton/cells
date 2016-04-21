;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

(See defpackage.lisp for license and copyright notigification)

|#

#| Notes

I don't like the way with-cc defers twice, first the whole thing and then when the
body finally runs we are still within the original integrity and each setf gets queued
to UFB separately before md-slot-value-assume finally runs. I think all that is going on here 
is that we want the programmer to use with-cc to show they know the setf will not be returning
a useful value. But since they have coded the with-cc we should be able to figure out a way to
let those SETFs thru as if they were outside integrity, and then we get a little less UFBing
but even better SETF behaves as it should.

It would be nice to do referential integrity and notice any time a model object gets stored in
a cellular slot (or in a list in such) and then mop those up on not-to-be.

|#

(in-package :cells)

(defparameter *c-prop-depth* 0)
(defparameter *causation* nil)

(defparameter *data-pulse-id* 0)
(define-symbol-macro .dpid *data-pulse-id*)
(defparameter *finbiz-id* 0) ;; debugging tool only
(define-symbol-macro .fbid *finbiz-id*)

(export! .dpid .fbid)
(defparameter *c-debug* nil)
(defparameter *defer-changes* nil)
(defparameter *within-integrity* nil)
(defvar *istack*)
(defparameter *client-queue-handler* nil)
(defparameter *unfinished-business* nil)
(defparameter *not-to-be* nil)

(defparameter *md-awake* nil)
(defparameter *md-awake-where* :anon)
(defparameter *ntb-dbg* nil)

(defun md-awake-ct ()
  (if *md-awake* (hash-table-count *md-awake*) 0))

(defun check-links (self where)
  (assert (not (cells-flushed self)))
  (print `(:model ,self ,where))
  (loop for (nil . c) in (cells self)
        do ;(print `(:cell ,c))
        (loop for c2 in (c-callers c)
            do (explore-caller c2))
        (loop for c2 in (c-useds c)
            do 
              (explore-used c2)))
  .bgo)

(defun explore-caller (c)
  (unless (gethash (c-model c) *md-awake*)
    (print `(:caller-outside? ,c))
    .bgo)
  (loop for u in (c-callers c)
      do (explore-caller u)))

(defun explore-used (c)
  (unless (gethash (c-model c) *md-awake*)
    (print `(:used-outside? ,c))
    .bgo)
  (loop for u in (c-useds c)
      do (explore-used u)))


(defmacro with-none-awake ((&key dbg diag) &body body)
  `(call-with-none-awake ,dbg (lambda () ,@body) ,diag))

(defun call-with-none-awake (dbg-info fn diag)
  (let ((*md-awake* (make-hash-table :test 'eq #-sbcl :weak-keys #-sbcl t)))
    (prog1
        (funcall fn)
      (when (md-awakep)
        (if diag
            (md-awake-map diag)
          (progn
            (print dbg-info)
            (md-awake-dump)))
        (break "some awake ~a" dbg-info)))))

(defun md-awake-record (self &optional (where *md-awake-where*))
  (when *md-awake*
    ;;(trcx md-awake-record self where)
    (setf (gethash self *md-awake*) where)))

(defun md-awake-remove (self)
  (when *md-awake*
    (remhash self *md-awake*)))

(export! md-awake-dump md-awakep md-awake-remove md-awake-record with-none-awake
  md-awake-dump-one *md-awake-where* md-awake-map)


(defun md-awakep ()
  (plusp (md-awake-ct)))
(defun md-awake-dump ()
  (let ((j (make-hash-table :test 'equal)))
    (loop for x  being the hash-keys of *md-awake*
        using (hash-value where)
        do (incf (gethash (list where (type-of x)) j 0)))
    (maphash (lambda (k v)
               (print (list "awake" k v))) j)
    (loop for x  being the hash-keys of *md-awake*
        using (hash-value where)
        do (md-awake-dump-one x where))))

(defun md-awake-map (fn)
  (loop for x  being the hash-keys of *md-awake*
      using (hash-value where)
      do (funcall fn x where)))

(defmethod md-awake-dump-one (x y)(declare (ignore x y)))

(defun cells-reset (&optional client-queue-handler &key debug)
  (utils-kt-reset)
  (setf 
   *c-debug* debug
   *c-prop-depth* 0
   *not-to-be* nil
   *ntb-dbg* nil
   *data-pulse-id* 0
   *finbiz-id* 0
   *defer-changes* nil ;; should not be necessary, but cannot be wrong
   *client-queue-handler* client-queue-handler
   *within-integrity* nil
   *unfinished-business* nil
   *trcdepth* 0)
  #-its-alive! (md-census-start)
  (trc nil "------ cell reset ----------------------------"))

#+xx
(cells-reset)

(defparameter *c-stopper* 'c-stopper)

(defun c-stop (&optional why)
  (funcall *c-stopper* why))



(defun c-stopper (why)
  (setf *stop* t)
  (print `(c-stop-entry ,why))
  (format t "~&C-STOP> stopping because ~a" why)  )

(define-symbol-macro .stop
    (c-stop :user))

(defun c-stopped ()
  *stop*)

(export! .stopped .cdbg)

(define-symbol-macro .cdbg
    *c-debug*)

(define-symbol-macro .stopped
    (c-stopped))

(defmacro c-assert (assertion &optional places fmt$ &rest fmt-args)
  (declare (ignorable assertion places fmt$ fmt-args))
   #+(or)`(progn) 
  `(unless *stop*
     (unless ,assertion
       ,(if fmt$
            `(c-break ,fmt$ ,@fmt-args)
          `(c-break "failed assertion: ~a" ',assertion)))))

(defvar *call-stack* nil)
(defvar *depender* nil)
;; 2008-03-15: *depender* let's us differentiate between the call stack and
;; and dependency. The problem with overloading *call-stack* with both roles
;; is that we miss cyclic reentrance when we use without-c-dependency in a 
;; rule to get "once" behavior or just when fm-traversing to find someone

(defmacro def-c-trace (model-type &optional slot cell-type)
  `(defmethod trcp ((self ,(case cell-type
                             (:c? 'c-dependent)
                             (otherwise 'cell))))
     (and (typep (c-model self) ',model-type)
       ,(if slot
            `(eq (c-slot-name self) ',slot)
          `t))))

(defmacro without-c-dependency (&body body)
  ` (let (*depender*)
      ,@body))

(export! .cause)

(define-symbol-macro .cause
    (car *causation*))

(define-condition unbound-cell (unbound-slot)
  ((cell :initarg :cell :reader cell :initform nil)))

(defparameter *observe-why* nil) ;; debug aid

(defgeneric slot-value-observe (slotname self new old old-boundp cell)
  #-(or cormanlisp)
  (:method-combination progn))

#-cells-testing
(defmethod slot-value-observe #-(or cormanlisp) progn
  (slot-name self new old old-boundp cell)
  (declare (ignorable slot-name self new old old-boundp cell)))

#+hunh
(fmakunbound 'slot-value-observe)
; -------- cell conditions (not much used) ---------------------------------------------

(define-condition xcell () ;; new 2k0227
  ((cell :initarg :cell :reader cell :initform nil)
   (app-func :initarg :app-func :reader app-func :initform 'bad-cell)
   (error-text :initarg :error-text :reader error-text :initform "<???>")
   (other-data :initarg :other-data :reader other-data :initform "<nootherdata>"))
  (:report (lambda (c s)
             (format s "~& trouble with cell ~a in function ~s,~s: ~s"
               (cell c) (app-func c) (error-text c) (other-data c)))))

(define-condition c-enabling ()
   ((name :initarg :name :reader name)
    (model :initarg :model :reader model)
    (cell :initarg :cell :reader cell))
   (:report (lambda (condition stream)
                 (format stream "~&unhandled <c-enabling>: ~s" condition)
                 (break "~&i say, unhandled <c-enabling>: ~s" condition))))

(define-condition c-fatal (xcell)
   ((name :initform :anon :initarg :name :reader name)
    (model :initform nil :initarg :model :reader model)
    (cell :initform nil :initarg :cell :reader cell))
   (:report (lambda (condition stream)
              (format stream "~&fatal cell programming error: ~s" condition)
              (format stream "~&  : ~s" (name condition))
              (format stream "~&  : ~s" (model condition))
              (format stream "~&  : ~s" (cell condition)))))


(define-condition asker-midst-askers (c-fatal)
  ())
;; "see listener for cell rule cycle diagnotics"

(define-condition c-unadopted (c-fatal) ()
   (:report
    (lambda (condition stream)
      (format stream "~&unadopted cell >: ~s" (cell condition))
      (format stream "~& >: often you mis-edit (c? (c? ...)) nesting is error"))))

(defun c-break (&rest args)
  (unless *stop*
    (let ((*print-level* 5)
          (*print-circle* t)
          (args2 (mapcar 'princ-to-string args)))
      (c-stop :c-break)
      ;(format t "~&c-break > stopping > ~{~a ~}" args2)
      (apply 'error args2))))