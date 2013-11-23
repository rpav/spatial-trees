;;; -*- mode: lisp -*-

(asdf:defsystem :spatial-trees-viz
  :depends-on (:spatial-trees
               :spatial-trees-test
               :mcclim)
  :components
  ((:file "spatial-tree-viz")))

