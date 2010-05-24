;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-user; -*-
;;;
;;; Copyright © 1995,2003 by Kenneth William Tilton.
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

(defpackage #:cells-build-package
  (:use #:cl))

(in-package #:cells-build-package)

(defun build-sys (system$ &key source-directory force)
  (let (
        ;;; --------------------------------------
        ;;; Step 2: Implementation-specific issues
        ;;; 
        ;;; Let's assume this is fixed in CMUCL 19a, and fix it later if need be.
        #+cmu18
        (ext:*derive-function-types* nil)
        
        #+lispworks
        (hcl::*handle-existing-defpackage* (list :add))
	)

    ;;----------------------------------------
    ;; source-directory validation...
    ;;
    (assert (pathnamep source-directory)
	    (source-directory)
	    "source-directory not supplied, please edit build.lisp to specify the location of the source.")
  (let ((project-asd (merge-pathnames (format nil "~a.asd" system$)
                         source-directory)))
      (unless (probe-file project-asd)
        (error "~a not found. revise build.lisp if asd file is somewhere else." project-asd)))
    
    ;;;----------------------------------
    ;;; ok. build...
    ;;;
    (push source-directory asdf:*central-registry*)
    (asdf:operate 'asdf:load-op (intern system$ :keyword) :force force)))