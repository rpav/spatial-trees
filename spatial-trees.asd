;;; -*- mode: lisp -*-
(asdf:defsystem :spatial-trees
  :components
  ((:module base
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "package")
             (:file "basedefs" :depends-on ("package"))
             (:file "rectangles" :depends-on ("package"))))
   (:module tree-impls
            :depends-on (base)
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "r-trees")
             (:file "greene-trees" :depends-on ("r-trees"))
             (:file "rstar-trees" :depends-on ("r-trees"))
             (:file "rplus-trees" :depends-on ("r-trees"))
             (:file "x-trees" :depends-on ("r-trees" "rstar-trees"))))
   (:module viz
            :if-component-dep-fails :ignore
            :depends-on (base)
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "r-tree-viz"
                    :in-order-to
                    ((compile-op (feature :clim))
                     (load-op (feature :clim))))))
   (:module tests
            :depends-on (base)
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:static-file "spatial-tree-test"
                           :pathname #p"spatial-tree-test.lisp")))
   (:static-file "LICENCE")
   (:static-file "TODO")))
