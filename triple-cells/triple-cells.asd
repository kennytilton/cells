
(in-package #:cl-user)

(defpackage #:t3c-system
    (:use #:cl #:asdf))

(in-package #:t3c-system)

(defsystem :triple-cells
    :name "Triple Cells"
  :description "An Integration of Cells and RDF"
  :version "0.1"
  :maintainer "Kenneth William Tilton"
  :licence "Commercial"
  :serial t
  :encoding :utf-8
  :depends-on (:utils-kt :cells)
  :components ((:static-file "triple-cells.asd")
               (:file "defpackage")
               (:file "ag-utilities")
               (:file "3c-integrity")
               (:file "core")
               (:file "api")
               (:file "dataflow")
               (:file "observer")
               (:file "hello-world")))
