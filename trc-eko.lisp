;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    The Newly Cells-aware TRC trace and EKO value echo facilities

(See defpackage.lisp for license and copyright notigification)

|#

(in-package :cells)

;----------- trc -------------------------------------------
(defparameter *last-trc* (get-internal-real-time))
(defparameter *trcdepth* 0)

(defun trcdepth-reset ()
  (setf *trcdepth* 0))



(defmacro trc (tgt-form &rest os)
  (if (eql tgt-form 'nil)
      '(progn)
    (if (stringp tgt-form)
        `(without-c-dependency
          (call-trc t ,tgt-form ,@os))
      (let ((tgt (gensym)))
        ;(break "slowww? ~a" tgt-form)
        `(without-c-dependency
          (bif (,tgt ,tgt-form)
            (if (trcp ,tgt)
                (progn
                  (assert (stringp ,(car os)) () "trc with test expected string second, got ~a" ,(car os))
                  (call-trc t ,@os)) ;;,(car os) ,tgt ,@(cdr os)))
              (progn
                ;(trc "trcfailed")
                (count-it :trcfailed)))
            (count-it :tgtnileval)))))))

(defparameter *trc-path-id* nil)

(defun call-trc (stream s &rest os)
  ;(break)
  (let ((path (cond
               (*trc-path-id*)
               ((and (boundp '*trcdepth*)
                  *trcdepth*)
                (format nil "~v,,,'.<~d~>> " (mod *trcdepth* 100) *trcdepth*))
               (""))))
    (format stream "~&~a: " path))
  ;;(format stream " ~a " (round (- (get-internal-real-time) *last-trc*) 10))
  (setf *last-trc* (get-internal-real-time))
  (format stream "~a" s)
  (let (pkwp)
    (dolist (o os)
      (format stream (if pkwp " ~(~s~)" " ~(~s~)") o) ;; save, used to insert divider, trcx dont like
      (setf pkwp (keywordp o))))
  (force-output stream)
  (values))

(export! brk brkx .bgo bgo *trc-path-id* ntrcx)

(define-symbol-macro .bgo (break "go"))

(defmacro bgo (msg)
  `(break "BGO ~a" ',msg))

(defmacro brkx (msg)
  `(break "At ~a: OK?" ',msg))

(defmacro trcx (tgt-form &rest os)
  (if (eql tgt-form 'nil)
      '(progn)
    `(without-c-dependency
         (call-trc t ,(format nil "TX> ~(~s~)" tgt-form)
           ,@(loop for obj in (or os (list tgt-form))
                   nconcing (list (intern (format nil "~a" obj) :keyword) obj))))))

(defmacro ntrcx (&rest os)
  (declare (ignore os))
  '(progn))
  
(defun call-trc-to-string (fmt$ &rest fmt-args)
    (let ((o$ (make-array '(0) :element-type 'base-char
                :fill-pointer 0 :adjustable t)))
      (with-output-to-string (os-stream o$)
        (apply 'call-trc os-stream fmt$ fmt-args))
      o$))

#+findtrcevalnils
(defmethod trcp :around (other)
  (unless (call-next-method other)(break)))

(defmethod trcp (other)
  (eq other t))

(defmethod trcp (($ string))
  t)
  


(defmacro wtrc ((&optional (min 1) (max 50) &rest banner) &body body )
  `(let ((*trcdepth* (if *trcdepth*
                         (1+ *trcdepth*)
                       0)))
     ,(when banner `(when (>= *trcdepth* ,min)
                      (if (< *trcdepth* ,max)
                          (trc ,@banner)
                        (progn
                          (break "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
                          nil))))
     (when (< *trcdepth* ,max)
       ,@body)))

(defmacro wtrcx ((&key (min 1) (max 50) (on? t))(&rest banner) &body body )
  `(let ((*trcdepth* (if *trcdepth*
                         (1+ *trcdepth*)
                       0)))
     ,(when banner `(when (and ,on? (>= *trcdepth* ,min))
                      (if (< *trcdepth* ,max)
                          (trc ,@banner)
                        (progn
                          (break "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
                          nil))))
     (when (< *trcdepth* ,max)
       ,@body)))

(defmacro wnotrc ((&optional (min 1) (max 50) &rest banner) &body body )
  (declare (ignore min max banner))
  `(progn ,@body))
  
;------ eko --------------------------------------

(defmacro eko ((&rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
        ,(if (stringp (car trcargs))
             `(trc ,(car trcargs) :=> ,result ,@(cdr trcargs))
           `(trc ,(car trcargs) ,(cadr trcargs) :=> ,result ,@(cddr trcargs)))
         ,result)))

(defmacro ekx (ekx-id &rest body)
  (let ((result (gensym)))
     `(let ((,result (,@body)))
         (trc ,(string-downcase (symbol-name ekx-id)) :=> ,result)
         ,result)))

(defmacro eko-if ((&rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
         (when ,result
           (trc ,(car trcargs) :res ,result ,@(cdr trcargs)))
         ,result)))

(defmacro ek (label &rest body)
  (let ((result (gensym)))
     `(let ((,result (,@body)))
         (when ,label
           (trc ,label ,result))
         ,result)))

