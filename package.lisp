(cl:defpackage "SPATIAL-TREES"
  (:use "CL")
  (:shadow "DELETE" "SEARCH")
  (:export "DELETE" "INSERT" "SEARCH"
           "MAKE-SPATIAL-TREE"))

(cl:defpackage "SPATIAL-TREES-PROTOCOL"
  (:use "CL" "SPATIAL-TREES")
  (:shadowing-import-from "SPATIAL-TREES" "DELETE" "SEARCH")
  (:export
   ;; interface definitions
   "DELETE" "INSERT" "SEARCH"
   ;; protocol functions
   "CHOOSE-LEAF" "SPLIT-NODE"
   ;; protocol classes
   "TREE" "NODE" "LEAF-NODE"
   ))

(cl:defpackage "RECTANGLES"
  (:use "CL")
  (:shadow "INTERSECTION")
  (:export "RECTANGLE" "MAKE-RECTANGLE" "INTERSECTION" "INTERSECTP"
           "AREA" "MINIMUM-BOUND" "LOWS" "HIGHS"))

(cl:defpackage "SPATIAL-TREES-IMPL"
  (:use "CL" "SPATIAL-TREES" "RECTANGLES")
  (:shadowing-import-from "SPATIAL-TREES" "DELETE" "SEARCH")
  (:shadowing-import-from "RECTANGLES" "INTERSECTION"))

