
(defpackage :spatial-trees.nns
  (:use :cl
        :rectangles
        :spatial-trees-impl
        :optima
        :alexandria
        :iterate)
  (:shadow :intersection))

(in-package :spatial-trees.nns)

;; Nearest Neighbor Queries
;; N Roussopoulos, S Kelley, F Vincent - ACM sigmod record, 1995

(defun ^2 (x) (* x x))

(defun mindist (coordinates r)
  (declare (type list coordinates))
  (declare (type rectangle r))
  (reduce #'+
          (map 'vector
               (lambda (s_i t_i p_i)
                 (^2 (- (clamp p_i s_i t_i) p_i)))
               (lows r)
               (highs r)
               coordinates)))

(defun minimax-dist (coordinates r)
  (declare (type list coordinates))
  (declare (type rectangle r))
  (flet ((rmk (s_k t_k p_k)
           (if (<= p_k (/ (+ s_k t_k) 2)) s_k t_k))
         (rMi (s_i t_i p_i)
           (if (>= p_i (/ (+ s_i t_i) 2)) s_i t_i)))
    
    (let ((indices (iota (length coordinates))))
      (reduce #'min
              (map 'vector
                   (lambda (s_k t_k p_k k)
                     (+ (^2 (- p_k (rmk s_k t_k p_k)))
                        (reduce #'+
                                (map 'vector
                                     (lambda (s_i t_i p_i i)
                                       (if (= i k)
                                           0
                                           (^2 (- p_i (rMi s_i t_i p_i)))))
                                     (lows r)
                                     (highs r)
                                     coordinates
                                     indices))))
                   (lows r)
                   (highs r)
                   coordinates
                   indices)))))

(defun nearest-neighbor-search (point tree distance-function)
  (ematch tree
    ((rectangle root)
     (%nns root point (list nil MOST-POSITIVE-DOUBLE-FLOAT)
           distance-function))))

(defun %nns (node point nearest fn)
  (match node
    ((spatial-tree-leaf-node children)
     (reduce (lambda (best current)
               (match best
                 ((list _ mindist)
                  (let ((dist (funcall fn point current)))
                    (if (< dist mindist)
                        (list current dist)
                        best)))))
             children
             :initial-value nearest))
    ((spatial-tree-node children)
     
     ;; I don't like the procedual style but I try to follow the
     ;; notation in the paper
     
     (iter (with last = (prune-downward
                         node point nearest
                         (sort children #'< :key (curry point #'mindist))))
           (for new-node in last)
           (setf nearest (%nns new-node point nearest fn))
           (setf last
                 (prune-upward
                  node point nearest
                  last))
           (finally (return nearest))))))

(defun prune-downward (node point nearest abl)
  )

(defun prune-upward (node point nearest abl)
  )