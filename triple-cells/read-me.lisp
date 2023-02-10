#|

               Triple-Cells: (+ RDF Cells)
               ---------------------------

Prerequisites
-------------
Cells: http://common-lisp.net/project/cells/

  Lotsa broken links. Use c-l.net repsoitories access to get to CVS:

    http://common-lisp.net/cgi-bin/viewcvs.cgi/?root=cells


RDF: http://www.w3.org/RDF/

  That is the RDF standard. Many implementations available, even from Oracle. Redland is an open one.

Redland: http://librdf.org/

  C, open, lotsa bindings to other languages, Lisp bindings and port of triple-cells 
  left as an exercise. I use AllegroCL/Allegrograph.

Free trial AG: http://www.franz.com/downloads/clp/agle_survey

  It is not clear whether you first need to download/install the free express edition
  of AllegroCL or whether this download does it all.

Download of Triple-Cells itself
-------------------------------
  Start here: http://common-lisp.net/cgi-bin/viewcvs.cgi/?root=cells

  Then you need both Cells and triple-cells. Getting Cells just requires the contained
  utils-kt, but my favorite debug stuff is Cells-aware so resides there. Gotta refactor someday.

  hello-world.lisp includes a function 3c-test. Once that or 3c-test-build has been run, more fun
  is 3c-test-reopen, which shows the AG database has all the information needed to "run" a 
  database, assuming triple-cells is loaded.

|#