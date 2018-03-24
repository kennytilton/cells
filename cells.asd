;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl ecl ccl abcl clasp)
(progn
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(asdf:defsystem :cells
  :name "cells"
  :author "Kenny Tilton <kentilton@gmail.com>"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :licence "Lisp LGPL"
  :description "Cells"
  :long-description "Cells: a dataflow extension to CLOS."
  :version "3.0"
  :serial t
  :depends-on (:utils-kt)
  :components ((:file "defpackage")
               (:file "trc-eko")
               (:file "cells")
               (:file "integrity")
               (:file "cell-types")
               (:file "constructors")
               (:file "initialize")
               (:file "md-slot-value")
               (:file "slot-utilities")
               (:file "link")
               (:file "propagate")
               (:file "synapse")
               (:file "synapse-types")
               (:file "model-object")
               (:file "defmodel")
               (:file "md-utilities")
               (:file "family")
               (:file "fm-utilities")
               (:file "family-values")
	       (:file "test-propagation")
	       (:file "cells-store")))

(defmethod perform ((o load-op) (c (eql (find-system :cells))))
  (pushnew :cells *features*))

(defmethod perform ((o test-op) (c (eql (find-system :cells))))
  (oos 'load-op :cells-test))

(defmethod perform ((o test-op) (c (eql :cells)))
  (oos 'load-op :cells-test)))
