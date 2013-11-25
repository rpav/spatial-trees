(asdf:defsystem :spatial-trees.nns
  :depends-on (:spatial-trees
               :optima
               :alexandria
               :iterate)
  :components
  ((:file :nearest-search)))
