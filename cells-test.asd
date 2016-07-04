;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :cells-test
    :name "cells-test"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT Style"
  :description "Cells Regression Test/Documentation"
  :long-description "Informatively-commented regression tests for Cells"
  :serial t
  :depends-on (:cells)
  :components ((:module "cells-test"
                 :serial t
                 :components ((:file "test")
                              (:file "hello-world")
                              (:file "test-kid-slotting")
                              (:file "test-lazy")
                              (:file "person")
                              (:file "df-interference")
                              (:file "test-family")
                              (:file "output-setf")
                              (:file "test-cycle")
                              (:file "test-ephemeral")
                              (:file "test-synapse")
                              (:file "deep-cells")))))

(defmethod perform :after ((op load-op) (system (eql (find-system :cells-test))))
  (funcall (find-symbol "TEST-CELLS" "CELLS")))

