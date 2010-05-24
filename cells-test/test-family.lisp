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

(defmodel human (family)
  ((age :initarg :age :accessor age :initform 10)))

(defobserver .kids ((self human))
  (when new-value
    (print `(i have ,(length new-value) kids))
    (dolist (k new-value)
      (trc "one kid is named" (md-name k) :age (age k)))))
  
(defobserver age ((k human))
  (format t "~&~a is ~d years old" (md-name k) (age k)))
  
(def-cell-test cv-test-family ()
  (cells-reset)
  (let ((mom (make-instance 'human)))
    ;
    ; the real power of cells appears when a population of model-objects are linked by cells, as
    ; when a real-word collection of things all potentially affect each other.
    ;
    ; i use the family class to create a simple hierarchy in which kids have a pointer to their
    ; parent (.fm-parent, accessor fm-parent) and a parent has a cellular list of their .kids (accessor kids)
    ;
    ; great expressive power comes from having kids be cellular; the model population changes as
    ; the model changes in other ways. but this creates a delicate timing problem: kids must be fully
    ; spliced into the model before their ruled cellular slots can be accessed, because a cell rule
    ; itself might try to navigate the model to get to a cell value of some other model-object.
    ;
    ; the cell engine handles this in two steps. first, deep in the state change handling code
    ; the .kids slot gets special handling (this is new for 2002, and come to think of it i will
    ; have to expose that hook to client code so others can create models from structures other
    ; than family) during which the fm-parent gets populated, among other things. second, the output of
    ; kids calls to-be on each kid.
    ;
    ; one consequence of this is that one not need call to-be on new instances being added to
    ; a larger model family, it will be done as a matter of course.
    ;    
    (push (make-instance 'human :fm-parent mom :md-name 'natalia :age (c-in 23)) (kids mom))
    (push (make-instance 'human :fm-parent mom :md-name 'veronica :age (c? (- (age (fm-other natalia)) 6))) (kids mom))
    (push (make-instance 'human :fm-parent mom :md-name 'aaron :age (c? (- (age (fm-other veronica)) 4))) (kids mom))
    (push (make-instance 'human :fm-parent mom :md-name 'melanie :age (c? (- (age (fm-other veronica)) 12))) (kids mom))
    ;
    ; some of the above rules invoke the macro fm-other. that searches the model space, first searching the 
    ; kids of the starting point (which defaults to a captured 'self), then recursively up to the 
    ; parent and the parent's kids (ie, self's siblings)
    ;
    (flet ((nat-age (n)
             (setf (age (fm-other natalia :starting mom)) n)
             (dolist (k (kids mom))
               (ct-assert
                (eql (age k)
                  (ecase (md-name k)
                    (natalia n)
                    (veronica (- n 6))
                    (aaron (- n 10))
                    (melanie (- n 18))))))))
      (nat-age 23)
      (nat-age 30)
      (pop (kids mom))
      (nat-age 40))))

#+(or)

(cv-test-family)
    
;------------ family-values ------------------------------------------
;;; 
;;; while family-values is itself rather fancy, the only cell concept introduced here
;;; is that cell rules have convenient access to the current value of the slot, via
;;; the symbol-macro ".cache" (leading and trailing full-stops). to see this we need to
;;; go to the definition of family-values and examine the rule for the kids cell:
;;;
;;;           (c? (assert (listp (kidvalues self)))
;;;               (eko (nil "gridhost kids")
;;;                    (let ((newkids (mapcan (lambda (kidvalue)
;;;                                               (list (or (find kidvalue .cache :key (kvkey self) :test (kvkeytest self))
;;;                                                         (trc nil "family-values forced to make new kid" self .cache kidvalue)
;;;                                                         (funcall (kidfactory self) self kidvalue))))
;;;                                     (^kidvalues))))
;;;                      (nconc (mapcan (lambda (oldkid)
;;;                                         (unless (find oldkid newkids)
;;;                                           (when (fv-kid-keep self oldkid)
;;;                                             (list oldkid))))
;;;                               .cache)
;;;                             newkids))))
;;; 
;;; for efficiency's sake, family-values (fvs) generate kids only as needed based on determining
;;; kidvalues cell. wherever possible existing kids are kept. this is done by looking in the current
;;; value of the kids slot for a kid matching each new kidvalue and reusing that. we cannot use the
;;; accessor kids because the first time thru the cell is internally invalid, so the rule will get dispatched
;;; again in an infinite loop if we go through the accessor protocol.
;;;
;;; mind you, we could just use slot-value; .cache is just a convenience.
;;;
(defmodel bottle (model)
  ((label :initarg :label :initform "unlabeled" :accessor label)))

#+(or)
(cv-family-values)

(def-cell-test cv-family-values ()
  (let* ((kf-calls 0)
         (wall (make-instance 'family-values
                        :kv-collector (lambda (mdv)
                                       (eko ("kidnos")(when (numberp mdv)
                                         (loop for kn from 1 to (floor mdv)
                                              collecting kn))))
                        :value (c-in 5)
                        :kv-key #'value
                        :kid-factory (lambda (f kv)
                                      (incf kf-calls)
                                      (trc "making kid" kv)
                                      (make-instance 'bottle
                                        :fm-parent f
                                        :value kv
                                        :label (c? (format nil "bottle ~d out of ~d on the wall"
                                                       (^value)
                                                       (length (kids f)))))))))
    (ct-assert (eql 5 kf-calls))
   
    (setq kf-calls 0)
    (decf (value wall))
    (ct-assert (eql 4 (length (kids wall))))
    (ct-assert (zerop kf-calls))

    (setq kf-calls 0)
    (incf (value wall))
    (ct-assert (eql 5 (length (kids wall))))
    (ct-assert (eql 1 kf-calls))

    ))

#+(or)
(cv-family-values)
