
(in-package :cl-user)

(defpackage "SPATIAL-TREES"
  (:use "CL")
  (:shadow "DELETE" "SEARCH")
  (:export "DELETE" "INSERT" "SEARCH" "BOUNDING-RECTANGLE"
           "MAKE-SPATIAL-TREE"))

(defpackage "SPATIAL-TREES-PROTOCOL"
  (:use "CL" "SPATIAL-TREES")
  (:shadowing-import-from "SPATIAL-TREES" "DELETE" "SEARCH")
  (:export
   ;; interface definitions
   "DELETE" "INSERT" "SEARCH"
   ;; protocol functions
   "CHOOSE-LEAF" "SPLIT-NODE" "CHILDREN" "RECORDS" "ROOT-NODE"
   ;; protocol classes
   "SPATIAL-TREE" "SPATIAL-TREE-NODE" "SPATIAL-TREE-LEAF-NODE"
   ))

(defpackage "RECTANGLES"
  (:use "CL")
  (:shadow "INTERSECTION")
  (:export "RECTANGLE" "MAKE-RECTANGLE" "INTERSECTION" "INTERSECTP"
           "AREA" "MINIMUM-BOUND" "LOWS" "HIGHS"))

(defpackage "SPATIAL-TREES-IMPL"
  (:use "CL" "SPATIAL-TREES" "SPATIAL-TREES-PROTOCOL" "RECTANGLES")
  (:shadowing-import-from "SPATIAL-TREES" "DELETE" "SEARCH" "BOUNDING-RECTANGLE")
  (:shadowing-import-from "RECTANGLES" "INTERSECTION"))
