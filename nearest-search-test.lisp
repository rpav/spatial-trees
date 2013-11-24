
(defpackage :spatial-trees.nns.test
  (:use :cl
        :rectangles
        :spatial-trees-impl
        :optima
        :alexandria
        :iterate
        :fiveam)
  (:shadow :intersection :fail))

(in-package :spatial-trees.nns.test)

(def-suite :spatial-trees.nns)
(in-suite :spatial-trees.nns)

