(in-package :cells)

(defvar *client-log*)
(defvar *obs-1-count*)

(defmodel deep ()
  ((cell-2 :cell :ephemeral :initform (c-in 'two) :accessor :cell-2)
   (cell-1 :initform (c? (list 'one (^cell-2) (^cell-3))) :accessor :cell-1)
   (cell-3 :initform (c-in 'c3-unset) :accessor :cell-3)))

(defobserver cell-1 ()
  (trc "cell-1 observer raw now enqueing client to run first. (new,old)=" new-value old-value)
  (with-integrity (:client 1)
    (trc "cell-1 :client now running" new-value (incf *obs-1-count*))
    (eko ("c1-obs->*client-log*: ")
      (setf *client-log* (list new-value)))))

(defobserver cell-2 ()
  (trc "cell-2 observer raw now enqueing change and client to run second. (new,old)=" new-value old-value)
  (with-integrity (:change)
    (trc "cell-2 observer :change now running" *client-log*)
    (ct-assert (equal *client-log* '((one two c3-unset) two c3-unset))) 
    (setf (^cell-3) (case new-value (two 'three) (otherwise 'trouble))))
  (with-integrity (:client 2)
    (trc "client cell-2 :client running")
    (eko ("c2-obs->*client-log*: ")
      (setf *client-log* (append *client-log* (list new-value))))))

(defobserver cell-3 ()
  (trc "cell-3 observer raw now enqueing client to run third. (new,old)=" new-value old-value)
  (with-integrity (:client 3)
    (trc "cell-3 observer :client now running" new-value)
    (eko ("c3-obs->*client-log*: ")
      (setf *client-log* (append *client-log* (list new-value))))))

(defun deep-queue-handler (client-q)
  (loop for (defer-info . task) in (prog1
                                (sort (fifo-data client-q) '< :key 'car)
                              (fifo-clear client-q))
      do
        (trc nil "!!! --- deep-queue-handler dispatching" defer-info)
        (funcall task :user-q defer-info)))

(def-cell-test go-deep ()
  (cells-reset 'deep-queue-handler)
  (setf *obs-1-count* 0)
  (make-instance 'deep)
  (ct-assert (eql 2 *obs-1-count*)) ;; because the cell-2 observer does a setf on something used by c1
  (trc "testing *client-log*" *client-log*)
  (ct-assert (tree-equal *client-log* '((one nil three) three))))


    
