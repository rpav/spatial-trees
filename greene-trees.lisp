;;; Modification to the basic R-tree version of SPLIT-NODE to use
;;; Greene's algorithm from "An Implementation and Performance
;;; Analysis of Spatial Data Access Methods", Diane Greene, Proc. 5th
;;; IEEE Int. Conf. on Data Engineering, 1989.

(in-package "SPATIAL-TREES-IMPL")

(defclass greene-tree (r-tree)
  ())
(defmethod make-spatial-tree ((kind (eql :greene)) &rest initargs)
  (apply #'make-instance 'greene-tree
         :root-node (make-instance 'spatial-tree-leaf-node :records nil)
         initargs))

(defun choose-axis (entries node tree)
  (multiple-value-bind (seed1 seed2)
      (pick-seeds entries tree)
    (let ((lows1 (lows (mbr seed1 tree)))
          (highs1 (highs (mbr seed1 tree)))
          (lows2 (lows (mbr seed2 tree)))
          (highs2 (highs (mbr seed2 tree)))
          (mbr (if (slot-boundp node 'mbr)
                   (mbr node tree)
                   (minimum-bound-of (children node) tree))))
      (let ((max-dist (/ (if (> (car highs1) (car highs2))
                             (- (car lows1) (car highs2))
                             (- (car lows2) (car highs1)))
                         ;; erm, so what if a node has zero extent in
                         ;; a given dimension?
                         (- (car (highs mbr)) (car (lows mbr)))))
            (max-axis 0))
        (do ((lows1 (cdr lows1) (cdr lows1))
             (highs1 (cdr highs1) (cdr highs1))
             (lows2 (cdr lows2) (cdr lows2))
             (highs2 (cdr highs2) (cdr highs2))
             (lowsmbr (cdr (lows mbr)) (cdr lowsmbr))
             (highsmbr (cdr (highs mbr)) (cdr highsmbr))
             (axis 1 (1+ axis)))
            ((null lows1) max-axis)
          (let ((distance (/ (if (> (car highs1) (car highs2))
                                 (- (car lows1) (car highs2))
                                 (- (car lows2) (car highs1)))
                             (- (car highsmbr) (car lowsmbr)))))
            (when (> distance max-dist)
              (setf max-dist distance
                    max-axis axis))))))))

(defun distribute (entries axis node new-node tree)
  (let* ((length (length entries))
         (entries (sort entries #'<
                        :key (lambda (x) (nth axis (lows (mbr x tree)))))))
    (setf (children node) (subseq entries 0 (truncate length 2))
          (slot-value node 'mbr) (minimum-bound-of (children node) tree))
    (setf (children new-node) (subseq entries (truncate (1+ length) 2))
          (slot-value new-node 'mbr) (minimum-bound-of (children new-node) tree))
    (when (oddp length)
      (let ((elt (nth (truncate length 2) entries)))
        (let ((area1 (area (mbr node tree)))
              (area2 (area (mbr new-node tree))))
          (let* ((newmb1 (minimum-bound (mbr node tree) (mbr elt tree)))
                 (newmb2 (minimum-bound (mbr new-node tree) (mbr elt tree)))
                 (newarea1 (area newmb1))
                 (newarea2 (area newmb2)))
            (if (< (- newarea2 area2) (- newarea1 area1))
                (setf (children new-node) (cons elt (children new-node))
                      (slot-value new-node 'mbr) newmb2)
                (setf (children node) (cons elt (children node))
                      (slot-value node 'mbr) newmb1))))))))

(defmethod split-node ((tree greene-tree) new node)
  (let ((new-node (make-node-like node))
        (entries (cons new (children node))))
    (let ((axis (choose-axis entries node tree)))
      (distribute entries axis node new-node tree)
      new-node)))

#|

(let* ((list (loop repeat 1000 collect (make-random-rectangle)))
       (tree (make-spatial-tree :greene :rectfun #'identity)))
  (dolist (r list)
    (insert r tree))
  (let ((r (make-random-rectangle)))
    (unless (null (set-difference (search r tree)
                                  (remove-if-not (lambda (x) (intersectp x r)) list)
                                  :key (lambda (x)
                                         (list (lows x) (highs x)))
                                  :test #'equal))
      (error "aargh: ~S and ~S differ"
             (search r tree)
             (remove-if-not (lambda (x) (intersectp x r)) list)))))

(let* ((n 1000)
       (list (loop repeat n collect (make-random-rectangle)
                   collect (make-random-rectangle -2.0 -2.0)
                   collect (make-random-rectangle 2.0 2.0)))
       (tree (make-spatial-tree :greene :rectfun #'identity)))
  (dolist (r list)
    (insert r tree))
  (let ((r (make-rectangle :lows '(0.0 0.0) :highs '(1.0 1.0))))
    (time (search r tree))
    ;; this should probably be a shade less than three times as slow
    ;; as the tree-based search.
    (time (remove-if-not (lambda (x) (intersectp x r)) list))
    (assert (= (length (search r tree)) n))))

(let* ((n 100)
       (list (loop repeat n
                   for r = (make-random-rectangle)
                   collect (cons (lows r) (highs r))))
       (rectfun (lambda (x) (make-rectangle :lows (car x) :highs (cdr x))))
       (tree (make-spatial-tree :greene :rectfun rectfun)))
  (dolist (r list)
    (insert r tree))
  (let ((r (make-random-rectangle)))
    (unless (null (set-difference (search r tree)
                                  (remove-if-not
                                   (lambda (x)
                                     (intersectp (funcall rectfun x) r))
                                   list)
                                  :key (lambda (x)
                                         (let ((r (funcall rectfun x)))
                                           (list (lows r) (highs r))))
                                  :test #'equal))
      (error "aargh: ~S and ~S differ"
             (search r tree)
             (remove-if-not (lambda (x)
                              (intersectp (funcall rectfun x) r))
                            list)))))

(let* ((n 100)
       (list (loop repeat n collect (make-random-rectangle)))
       (tree (make-spatial-tree :greene :rectfun #'identity)))
  (dolist (r list)
    (insert r tree))
  (dolist (r (cdr list))
    (delete r tree))
  (unless (= (length (search (car list) tree)) 1)
    (error "aargh: wrong amount of stuff in ~S" tree)))

|#
