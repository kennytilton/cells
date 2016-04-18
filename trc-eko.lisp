;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    The Newly Cells-aware trc trace and EKO value echo facilities

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
        ;(brk "slowww? ~a" tgt-form)
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
(defparameter *trc-path-id-filter* nil)
(defparameter *trc-path-max* 0)
(defparameter *trc-path-max-path* nil)
(defparameter *trc-suppress* nil)
(export! *trc-path-max* trcap)

(defmacro trcap (&rest args)
  `(let (*trc-path-id-filter*)
     (trc ,@args)))

(defun trc-pathp (path)
  (when (> (length path) *trc-path-max*)
    ;;(print `(new-*trc-path-max* ,(length path) ,path))
    (setf *trc-path-max* (length path)
      *trc-path-max-path* path))

  #+good-for-test-one
  (when (> (- *trc-path-max* (length path)) 4)
    (break "big path delta for new path ~s agin max ~a" path *trc-path-max-path*))

  (or (null *trc-path-id-filter*)
    (if (> (length path)(length *trc-path-id-filter*))
        (eql 0 (search *trc-path-id-filter* path ))
      (eql 0 (search path *trc-path-id-filter*)))))

(export! *trc-path-id-filter* trc-pathp *trc-suppress* trx-zero)

#-allegro-v9.0
(defun curr-thread ()
  (excl:current-thread))

#+allegro-v9.0
(defun curr-thread ()
  sys:*current-thread*)

(defun thread-pid ()
  (or
   ;;#+cthread-mess
   (b-when thd (curr-thread)
    ;(print `(thread-name ,(slot-value thd 'sys::name)))
    (b-when p (slot-value thd 'sys::process)
      (mp:process-os-id p)))
   0))

(defun thread-name ()
  ;;#+cthread-mess
  (b-when thd (curr-thread)
     (slot-value thd 'sys::name)))

(defparameter *trx-zero* 0)

(defun trx-zero ()
  (setf *trx-zero* (get-internal-real-time)))

(defun call-trc (stream s &rest os)
  ;; (break)
  
  (when *trc-suppress* (return-from call-trc))
  
  (let ((path (cond
               (*trc-path-id*
                (unless (trc-pathp *trc-path-id*)
                  (return-from call-trc))
                *trc-path-id*)
               ((and (boundp '*trcdepth*)
                  *trcdepth*)
                (format nil "~v,,,'.<~d~>> " (mod *trcdepth* 100) *trcdepth*))
               (""))))
    (format stream "~&~@[~a ~]~@[~a:~]~@[<~a> ~]~a: "
      (round (- (get-internal-real-time) *trx-zero*) 100)
      nil #+slow? (left$ (thread-name) 8)
      nil #+xxx .dpid path))
  ;;(format stream " ~a " (round (- (get-internal-real-time) *last-trc*) 10))
  (setf *last-trc* (get-internal-real-time))

  (format stream "~a ~{~s ~}~%" s os)
  
  (force-output stream)
  (values))


(export! trx trx! brk brkx .bgo bgo *trc-path-id* ntrcx *trx-tag*)

(define-symbol-macro .bgo (brk "go"))

(defmacro bgo (msg)
  `(brk "BGO ~a" ',msg))

(defmacro brkx (msg)
  `(brk "At ~a: OK?" ',msg))

(defmacro trcx (tgt-form &rest os)
  (if (eql tgt-form 'nil)
      '(progn)
    `(without-c-dependency
         (call-trc t (format nil "TX[~d]> ~(~s~)"
                        (thread-pid) ',tgt-form)
           ,@(loop for obj in (or os (list tgt-form))
                   nconcing (list (intern (format nil "~a" obj) :keyword) obj))))))

(defmacro trx! (tag &rest os)
  `(let ((*trc-suppress* nil))
     (trx ,tag ,@os)))


(defparameter *trx-tag* "")

(defmacro trx (tgt-form &rest os)
  (if (eql tgt-form 'nil)
      '(progn)
    `(without-c-dependency
         (call-trc t (format nil "> ~a" ;; "TX[~a]> ~a"
                       ;; *trx-tag* ;; (ukt::irt-mshh$)
                        ,(string tgt-form))
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
  (unless (call-next-method other)(brk)))

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
                          (brk "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
                          nil))))
     (when (< *trcdepth* ,max)
       ,@body)))

(defmacro wtrcx ((&key (min 1) (max 50) (on? t))(&rest banner) &body body )
  `(let ((*trcdepth* (if *trcdepth*
                         (1+ *trcdepth*)
                       0)))
     ,(when banner `(when (and (>= *trcdepth* ,min) ,on?)
                      (if (< *trcdepth* ,max)
                          (trc ,@banner)
                        (progn
                          (brk "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
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

