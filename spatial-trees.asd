;;; -*- mode: lisp -*-
(asdf:defsystem :spatial-trees
  :components
  ((:module base
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "package")
             (:file "basedefs" :depends-on ("package"))))
   (:module tree-impls
            :depends-on (base)
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "r-trees")
             (:file "greene-trees" :depends-on ("r-trees"))
             (:file "rstar-trees" :depends-on ("r-trees"))))
   (:module viz
            :if-component-dep-fails :ignore
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "r-tree-viz"
                    :in-order-to
                    ((compile-op (feature :clim) (load-op base))
                     (load-op (feature :clim) (load-op base))))))))
