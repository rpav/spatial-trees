;;; Modifications to the R*-tree as in "The X-tree: An Index Structure
;;; for High-Dimensional Data", Berchtold, Keim and Kriegel,
;;; Proc. 22th Int. Conf. on Very Large Databases, 1996

(in-package "SPATIAL-TREES-IMPL")

(defclass x-tree (r*-tree)
  ((max-overlap :initarg :max-overlap :reader max-overlap)))
(defmethod initialize-instance :after ((tree x-tree) &rest args)
  (declare (ignore args))
  (unless (slot-boundp tree 'max-overlap)
    (setf (slot-value tree 'max-overlap) 1/5)))
(defmethod make-spatial-tree ((kind (eql :x)) &rest initargs)
  (apply #'make-instance 'x-tree
         :root-node (make-instance 'spatial-tree-leaf-node :records nil)
         initargs))

(defclass x-tree-node (spatial-tree-node)
  ((split-tree :initarg :split-tree :accessor split-tree)))
(defmethod split-tree ((x spatial-tree-leaf-node))
  x)
;;; Do we actually need to keep track of what is and what isn't a
;;; supernode?  I'm not sure that we do...
(defclass x-tree-supernode (x-tree-node)
  ())
;;; FIXME: leaf supernodes (and leaf nodes) don't need a split tree.
(defclass x-tree-leaf-supernode (x-tree-supernode spatial-tree-leaf-node)
  ())

(defvar *split*)

(defun find-cons-with-leaf (object conses)
  (cond
    ((atom conses) nil)
    ((eq (car conses) object) conses)
    ((eq (cdr conses) object) conses)
    (t (or (find-cons-with-leaf object (car conses))
           (find-cons-with-leaf object (cdr conses))))))

(defun leaves-of-split-tree (split-tree)
  (cond
    ((atom split-tree) (list split-tree))
    (t (append (leaves-of-split-tree (car split-tree))
               (leaves-of-split-tree (cdr split-tree))))))

;;; Figure 7: X-tree Insertion Algorithm for Directory Nodes
(defmethod adjust-tree ((tree x-tree) node &optional new)
  (check (or (null new)
             (or (and (typep node 'spatial-tree-leaf-node)
                      (typep new 'spatial-tree-leaf-node))
                 (and (not (typep node 'spatial-tree-leaf-node))
                      (not (typep new 'spatial-tree-leaf-node)))))
         "oh dear")
  (cond
    ((eq node (root-node tree)) new)
    (t
     (setf (slot-value node 'mbr) (minimum-bound-of (children node) tree))
     (let ((parent (parent node)))
       (if new
           (cond
             ((< (length (children parent)) (max-per-node tree))
              (push new (children parent))
              (setf (parent new) parent)
              (let ((cons (find-cons-with-leaf node (split-tree parent))))
                (if (eq node (car cons))
                    (rplaca cons (cons node new))
                    (progn
                      (check (eq node (cdr cons)) "Aargh1")
                      (rplacd cons (cons node new)))))
              (adjust-tree tree parent))
             (t
              (let ((cons (find-cons-with-leaf node (split-tree parent))))
                (if (eq node (car cons))
                    (rplaca cons (cons node new))
                    (progn
                      (check (eq node (cdr cons)) "Aargh2")
                      (rplacd cons (cons node new)))))
              (let ((new-parent (let ((*split* node))
                                  (split-node tree new parent))))
                (dolist (child (children parent))
                  (setf (parent child) parent))
                (when new-parent
                  (dolist (child (children new-parent))
                    (setf (parent child) new-parent)))
                (adjust-tree tree parent new-parent))))
           (adjust-tree tree parent))))))

(defmethod insert ((o t) (tree x-tree))
  (let* ((entry (make-leaf-node-entry :datum o
                                      :rectangle (funcall (rectfun tree) o)))
         (node (choose-subtree entry (root-node tree) (height tree) tree)))
    (cond
      ((< (length (children node)) (max-per-node tree))
       (push entry (children node))
       (adjust-tree tree node))
      (t
       (let ((new-node (split-node tree entry node)))
         (let ((new (adjust-tree tree node new-node)))
           (when new
             (let ((new-root
                    (make-instance 'x-tree-node
                                   :children (list (root-node tree) new))))
               (setf (parent (root-node tree)) new-root
                     (split-tree new-root) (cons (split-tree (root-node tree))
                                                 (split-tree new))
                     (root-node tree) new-root
                     (parent new) new-root)))))))
    tree))

;;; Figure 8: X-tree Split Algorithm for Directory Nodes
(defmethod split-node ((tree x-tree) new node)
  (let ((new-node (make-node-like node))
        (entries (cons new (children node))))
    (let ((axis (choose-split-axis entries tree)))
      (multiple-value-bind (one two)
          (choose-split-index entries axis tree)
        (let* ((bone (minimum-bound-of one tree))
               (btwo (minimum-bound-of two tree))
               (intersection (intersection bone btwo))
               (mb (minimum-bound bone btwo)))
          (cond
            ((or (null intersection)
                 (< (area intersection)
                    (* (area mb) (max-overlap tree))))
             (setf (children node) one
                   (slot-value node 'mbr) bone)
             (when (> (length one) (max-per-node tree))
               (check (typep node 'x-tree-supernode) "AARGH"))
             (setf (children new-node) two
                   (slot-value new-node 'mbr) btwo)
             (when (> (length two) (max-per-node tree))
               (change-class new-node
                             (if (typep node 'spatial-tree-leaf-node)
                                 'x-tree-leaf-supernode
                                 'x-tree-supernode)))
             (when (< (length two) (max-per-node tree))
               (change-class new-node
                             (if (typep node 'spatial-tree-leaf-node)
                                 'spatial-tree-leaf-node
                                 'x-tree-node)))
             new-node)
            ((and (not (typep node 'spatial-tree-leaf-node))
                  (let ((split-tree (split-tree node)))
                    (destructuring-bind (one . two) split-tree
                      (let ((l1 (leaves-of-split-tree one))
                            (l2 (leaves-of-split-tree two)))
                        (if (find *split* l1)
                            (push new l1)
                            (progn
                              (check (find *split* l2) "Missing node!!")
                              (push new l2)))
                      (and (>= (min-per-node tree) (length l1))
                           (>= (min-per-node tree) (length l2))
                           (progn
                             (setf (children node) l1
                                   (slot-value node 'mbr) (minimum-bound-of l1 tree))
                             (setf (children new-node) l2
                                   (slot-value new-node 'mbr) (minimum-bound-of l2 tree))
                             new-node)))))))
            (t (change-class node
                             (etypecase node
                               (spatial-tree-leaf-node 'x-tree-leaf-supernode)
                               (spatial-tree-node 'x-tree-supernode)))
               (push new (children node))
               (when (not (typep node 'spatial-tree-leaf-node))
                 (let ((cons (find-cons-with-leaf *split* (split-tree node))))
                   (if (eq *split* (car cons))
                       (rplaca cons (cons *split* new))
                       (progn
                         (check (eq *split* (cdr cons)) "Aargh2")
                         (rplacd cons (cons *split* new))))))
               nil)))))))
            
(defun check-consistency (x-tree)
  (labels ((%check (node)
             (assert 
              (or (typep node 'spatial-tree-leaf-node)
                  (null (set-difference (children node)
                                        (leaves-of-split-tree split-tree
                                        (append (car (split-tree node))
                                                (cdr (split-tree node)))))))
             (unless (typep node 'spatial-tree-leaf-node)
               (dolist (child (children node))
                 (%check child)))))
    (%check (root-node x-tree))))