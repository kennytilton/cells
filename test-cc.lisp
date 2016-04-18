;; 

(in-package :cells)

(defmd tcc ()
  (tccversion 1)
  (tcc-a (c-in nil))
  (tcc-2a (c-in nil)))

(defobserver tcc-a ()
  (case (^tccversion)
    (1 (when new-value
         (with-cc :tcc-a-obs
           (setf (tcc-2a self) (* 2 new-value))
           (with-cc :aha!2
             (assert (eql (tcc-2a self) (* 2 new-value))
               () "one")
             (trc "one happy")))
         (with-cc :aha!
           (assert (eql (tcc-2a self) (* 2 new-value))
             () "two"))))
    (2 (when new-value
         (with-cc :tcc-a-obs
           (setf (tcc-2a self) (* 2 new-value))
           (with-cc :aha!2
             (assert (eql (tcc-2a self) (* 2 new-value))
               () "one")
             (trc "one happy")))))))


(defun test-with-cc ()
  (let ((self (make-instance 'tcc 
                 :tccversion 2 ;:tcc-2a
                )))
    (trcx cool 42)
    (setf (tcc-a self) 42)
    (assert (eql (tcc-2a self) 84))))

#+test
(test-with-cc)

(defmd ccproc () ccp obs drv)

(defobserver ccp ()
  (trcx obs-cpp new-value old-value)
  (with-cc :obs-cpp
    (setf (^obs) (+ (* 10 (^drv)) new-value))))

(dbgobserver obs)

(defun test-ccproc ()
  (cells-reset)
  (let ((x (make-instance 'ccproc
             :ccp (c-in 0)
             :obs (c-in 0)
             :drv (c? (+ 10 (^ccp))))))
    (trcx see-0-10 100 (ccp x)(drv x)(obs x))

    (setf (ccp x) 1)
    (trcx see-1-11-101 (ccp x)(drv x)(obs x))

    (trcx now-see-1-11-101 (ccp x)(drv x)(obs x))
    (setf (ccp x) 2)

    (trcx see-2-12-102 (ccp x)(drv x)(obs x))
    (trcx see-2-12-102 (ccp x)(drv x)(obs x))))
    