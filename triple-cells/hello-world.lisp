;; -*- mode: Lisp; Syntax: Common-Lisp; Package: triple-cells; -*-
;;;
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;


(in-package :3c)

#+test
(3c-test-reopen)

#+test
(3c-test)

(defun 3c-test ()
  (3c-test-build)
  (3c-test-reopen)
  )

(defun 3c-test-build ()
  (ukt:test-prep "3c")
  ;
  ; initialize new DB altogether
  ;
  (create-triple-store "hello-world"
    :if-exists :supersede
    :directory (project-path))
  (register-namespace "hw" "helloworld#" :errorp nil)
  (register-namespace "ccc" "triplecells#" :errorp nil)
  ;
  ; initialize new DB session
  ;
  (3c-init)
  
  (let ((*synchronize-automatically* t))
    (enable-print-decoded t)
    
    (make-observer !hw:echo-happen (trc "happen:" new-value))
    (make-observer !hw:location (trc "We are now" new-value ))
    (make-observer !hw:obs-response (trc "Speak:" new-value ))
    
    (with-3c-integrity (:change) ;; change advances pulse
      (let ((dell (3c-make !hw:computer  :id "dell"))
            (happen !hw:happen)
            (location !hw:location)
            (response !hw:response))
        (declare (ignorable response location))
        (assert dell)
        
        (stmt-new dell happen
          (3c-in  nil :ephemeral t
            :observer !hw:echo-happen
            :test 'equal))

        
        (stmt-new dell location
          (3c? (let ((h (3c (3c-find-id "dell") !hw:happen)))
                 ;(trc "rule sees happen" h)
                 (cond
                  ((string-equal h "arrive") "home")
                  ((string-equal h "leave") "away")
                  (cache? cache)
                  (t "away")))
            :observer !hw:location
            :test 'equal))
        ;;#+step2
        (progn
         
          (stmt-new dell response
            (3c? (let* ((dell (3c-find-id "dell"))
                        (h (3c dell !hw:happen))
                        (loc (3c dell !hw:location)))
                   ;(trc "response rule sees happen" h :loc loc)
                   (cond
                    ((string-equal h "knock-knock")
                     (cond
                      ((string-equal loc "home") "who's there?")
                      (t "silence")))
                    ((string-equal h "arrive")
                     (cond
                      ((string-equal loc "home") "honey, i am home!")))
                    ((string-equal h "leave")
                     (cond
                      ((string-equal loc "away") "bye-bye!")))
                    (t cache)))
              :observer !hw:obs-response
              :ephemeral t
              :test 'equal)))))))

(defun 3c-test-reopen ()
  (close-triple-store)
  (open-triple-store "hello-world" 
    :directory (project-path)
    :if-does-not-exist :error)
  (when (3c-integrity-managed?) (break "1"))
  (time
   (let ((dell (3c-find-id "dell"))
         (happen !hw:happen)
         (location !hw:location)
         (response !hw:response))
    
     (trc "---------------- start-------------------------- " (3c dell happen)(3c dell location)(3c dell response))
     (when (3c-integrity-managed?) (break "2"))
     (setf (3c dell happen) "knock-knock")
     (loop repeat 2 do
           (setf (3c dell happen) "knock-knock"))
     (setf (3c dell happen) "arrive")
     
     (setf (3c dell happen) "knock-knock")
     (setf (3c dell happen) "leave")
     )))

