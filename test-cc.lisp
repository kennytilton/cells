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
    (assert (and (numberp (tcc-2a self))
              (= (tcc-2a self) 84)))))

#+test
(test-with-cc)

