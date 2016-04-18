;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells)
(defmacro defmodel (class directsupers slotspecs &rest options)
  ;;(print `(defmodel sees directsupers ,directsupers using ,(or directsupers :model-object)))
  (assert (not (find class directsupers))() "~a cannot be its own superclass" class)
  `(progn
     (setf (get ',class :cell-types) nil)
     
     (setf (get ',class :model-ownings) nil)

     (setf (get ',class 'slots-excluded-from-persistence)
       (loop for slotspec in ',slotspecs
           unless (and (getf (cdr slotspec) :ps t)
                    (getf (cdr slotspec) :persistable t))
           collect (car slotspec)))
     (loop for slotspec in ',slotspecs
         do (destructuring-bind
                (slotname &rest slotargs
                  &key (cell t)      
                  &allow-other-keys)
                slotspec
              (declare (ignorable slotargs))
              (when cell
                (setf (md-slot-cell-type ',class slotname) cell))))
     ;; define slot macros before class so they can appear in
     ;; initforms and default-initargs 
     ,@(loop for slotspec in slotspecs
           nconcing (destructuring-bind
                        (slotname &rest slotargs
                          &key (cell t) (accessor slotname) reader
                          &allow-other-keys)
                        slotspec
                      (declare (ignorable slotargs ))
                      (when cell
                        (list (let* ((reader-fn (or reader accessor))
                                     (deriver-fn (intern$ "^" (symbol-name reader-fn))))
                                `(eval-when (:compile-toplevel :execute :load-toplevel)
                                   (unless (macro-function ',deriver-fn)
                                     (defmacro ,deriver-fn ()
                                       `(,',reader-fn self)))
                                   #+sbcl (unless (fboundp ',reader-fn)
                                            (defgeneric ,reader-fn (slot)))))))))
     
     ;
     ; -------  defclass ---------------  (^slot-value ,model ',',slotname)
     ;
     (prog1
         (defclass ,class ,(or directsupers '(model-object)) ;; now we can def the class
           ,(mapcar (lambda (s)
                      (list* (car s)
                        (let ((ias (cdr s)))
                          (remf ias :persistable)
                          (remf ias :ps)
                          ;; We handle accessor below
                          (when (getf ias :cell t)
                            (remf ias :reader)
                            (remf ias :writer)
                            (remf ias :accessor))
                          (remf ias :cell)
                          (remf ias :owning)
                          (remf ias :unchanged-if)
                          ias))) (mapcar #'copy-list slotspecs))
           (:documentation
            ,@(or (cdr (find :documentation options :key #'car))
                '("chya")))
           (:default-initargs ;; nil ok and needed: acl oddity in re not clearing d-i's sans this
               ,@(cdr (find :default-initargs options :key #'car)))
           (:metaclass ,(or (cadr (find :metaclass options :key #'car))
                          'standard-class)))
       
       (defmethod shared-initialize :after ((self ,class) slot-names &rest iargs &key)
         (declare (ignore slot-names iargs))
         ,(when (and directsupers (not (member 'model-object directsupers)))
            `(unless (typep self 'model-object)
               (error "If no superclass of ~a inherits directly
or indirectly from model-object, model-object must be included as a direct super-class in
the defmodel form for ~a" ',class ',class))))
       
       ;
       ; slot accessors once class is defined...
       ;
       ,@(mapcar (lambda (slotspec)
                   (destructuring-bind
                       (slotname &rest slotargs
                         &key (cell t) unchanged-if (accessor slotname) reader writer type
                         &allow-other-keys)
                       slotspec
                     
                     (declare (ignorable slotargs))
                     (when cell
                       (let* ((reader-fn (or reader accessor))
                              (writer-fn (or writer accessor))
                              )
                         `(progn
                            ,(when writer-fn
                               `(defmethod (setf ,writer-fn) (new-value (self ,class))
                                  (setf (md-slot-value self ',slotname)
                                    ,(if type
                                         `(coerce new-value ',type)
                                       'new-value))))
                            ,(when reader-fn
                               `(defmethod ,reader-fn ((self ,class))
                                  (md-slot-value self ',slotname)))
                            ,(when unchanged-if
                               `(def-c-unchanged-test (,class ,slotname) ,unchanged-if)))))))
           slotspecs))
     (loop for slotspec in ',slotspecs
         do (destructuring-bind
                (slotname &rest slotargs &key (cell t) owning &allow-other-keys)
                slotspec
              (declare (ignorable slotargs))
              (when (and cell owning)
                (setf (md-slot-owning-direct? ',class slotname) owning))))))

(export! defmd-canonicalize-slot)
(defun defmd-canonicalize-slot (slotname
                                &key
                                (cell nil cell-p)
                                (ps t ps-p)
                                (persistable t persistable-p)
                                (owning nil owning-p)
                                (type nil type-p)
                                (initform nil initform-p)
                                (initarg (intern (symbol-name slotname) :keyword))
                                (documentation nil documentation-p)
                                (unchanged-if nil unchanged-if-p)
                                (reader slotname reader-p)
                                (writer `(setf ,slotname) writer-p)
                                (accessor slotname accessor-p)
                                (allocation nil allocation-p))
  (list* slotname :initarg initarg
    (append
     (when cell-p (list :cell cell))
     (when ps-p (list :ps ps))
     (when persistable-p (list :persistable persistable))
     (when owning-p (list :owning owning))
     (when type-p (list :type type))
     (when initform-p (list :initform initform))
     (when unchanged-if-p (list :unchanged-if unchanged-if))
     (when reader-p (list :reader reader))
     (when writer-p (list :writer writer))
     (when (or accessor-p 
             (not (and reader-p writer-p)))
       (list :accessor accessor))
     (when allocation-p (list :allocation allocation))
     (when documentation-p (list :documentation documentation)))))

(defmacro defmd (class superclasses &rest mdspec)
  `(defmodel ,class (,@superclasses model)
     ,@(let (definitargs class-options slots)
         (loop with skip
             for (spec next) on mdspec
             if skip
             do (setf skip nil)
             else do (etypecase spec
                       (cons
                        (cond
                         ((keywordp (car spec))
                          (assert (find (car spec) '(:documentation :metaclass)))
                          (push spec class-options))
                         ((find (cadr spec) '(:initarg :type :ps :persistable :cell :initform :allocation :reader :writer :accessor :documentation))
                          (push (apply 'defmd-canonicalize-slot spec) slots))
                         (t ;; shortform (slotname initform &rest slotdef-key-values)
                          (push (apply 'defmd-canonicalize-slot
                                  (list* (car spec) :initform (cadr spec) (cddr spec))) slots))))
                       (keyword
                        (setf definitargs (append definitargs (list spec next)))
                        (setf skip t))
                       (symbol (push (list spec :initform nil
                                       :initarg (intern (symbol-name spec) :keyword)
                                       :accessor spec) slots)))
             finally
               (return (list* (nreverse slots)
                         (delete nil
                           (list* `(:default-initargs ,@definitargs)
                             (nreverse class-options)))))))))

    

#+test
(progn
  (defclass md-test-super ()())

  (defmd defmd-test (md-test-super)
    (aaa :cell nil :initform nil :initarg :aaa :accessor aaa) ;; defmd would have written the same
    (aa2 :documentation "hi mom")
    bbb
    (ccc 42 :allocation :class)
    (ddd (c-in nil) :cell :ephemeral)
    :superx 42 ;; default-initarg
    (:documentation "as if!")))



