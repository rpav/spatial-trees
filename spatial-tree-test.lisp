;;; Somewhat rudimentary tests of external functionality

(in-package :cl-user)
(defpackage spatial-trees-test
  (:use :cl
        :spatial-trees
        :spatial-trees-protocol
        :rectangles
        :fiveam)
  (:export :make-random-rectangle)
  (:shadowing-import-from :spatial-trees :delete :search :bounding-rectangle)
  (:shadowing-import-from :rectangles :intersection))

(in-package :spatial-trees-test)
(def-suite :spatial-trees)
(in-suite :spatial-trees)


(defvar *kinds* '(:r :greene :r* :x))

(defun make-random-rectangle (&optional (x-bias 0.0) (y-bias 0.0))
  (let* ((lx (+ (random 1.0) x-bias))
         (ly (+ (random 1.0) y-bias))
         (hx (+ (random 1.0) lx))
         (hy (+ (random 1.0) ly)))
    (make-rectangle :lows (list lx ly) :highs (list hx hy))))

(defun gen-rectangle ()
  (lambda ()
    (make-random-rectangle)))

(def-fixture tree-kind ()
  (let (seen)
    (for-all ((kind (apply #'gen-one-element *kinds*) (not (find kind seen))))
      (push kind seen)
      (&body))))

(test (:random-search :fixture tree-kind)
  (for-all ((list (gen-list :length (constantly 1000)
                            :elements (gen-rectangle))))
    (let ((tree (make-spatial-tree kind :rectfun #'identity)))
      (finishes
        (dolist (r list)
          (insert r tree)))
      (let* ((r (make-random-rectangle))
             (result (search r tree))
             (expected (remove-if-not (lambda (x) (intersectp x r)) list)))
          (is (null (set-difference result
                                    expected
                                    :key (lambda (x)
                                           (list (lows x) (highs x)))
                                    :test #'equal))
              "~&In random search for kind ~S: ~S and ~S differ"
              kind result expected)))))

(defun gen-trisected-rectangle ()
  (let ((i 0))
    (lambda ()
      (incf i)
      (case (mod i 3)
        (0 (make-random-rectangle 0.0 0.0))
        (1 (make-random-rectangle -2.0 -2.0))
        (2 (make-random-rectangle 2.0 2.0))))))

(test (:trisected-search :fixture tree-kind)
  (let ((n 1000))
    (for-all ((list (gen-list :length (constantly 1000)
                              :elements (gen-trisected-rectangle))))
      (let ((tree (make-spatial-tree kind :rectfun #'identity)))
        (dolist (r list)
          (insert r tree))
        (let ((r (make-rectangle :lows '(0.0 0.0) :highs '(1.0 1.0))))
          ;; FIXME: find a way to test the relative speed of the following
          ;; (sbcl-specifically if necessary).
          (search r tree)
          (remove-if-not (lambda (x) (intersectp x r)) list)
          (is (= (length (search r tree)) n)
              "In trisected search for kind ~S, ~s=~s and ~s=~s differ"
              kind '(length (search r tree)) (length (search r tree))
              'n n))))))

(test (:arbitrary-object-search :fixture tree-kind)
  (let* ((n 100)
         (list (loop repeat n
                  for r = (make-random-rectangle)
                  collect (cons (lows r) (highs r))))
         (rectfun (lambda (x) (make-rectangle :lows (car x) :highs (cdr x))))
         (tree (make-spatial-tree kind :rectfun rectfun)))
    (dolist (r list)
      (insert r tree))
    (let* ((r (make-random-rectangle))
           (result (search r tree))
           (expected (remove-if-not
                      (lambda (x) (intersectp (funcall rectfun x) r))
                      list)))
      (is (null (set-difference
                 result expected
                 :key (lambda (x)
                        (let ((r (funcall rectfun x)))
                          (list (lows r) (highs r))))
                 :test #'equal))
          "Arbitrary object search for kind ~S: ~S and ~S differ"
          kind result expected))))


(test (:deletion :fixture tree-kind)
  (let* ((n 100)
         (list (loop repeat n collect (make-random-rectangle)))
         (tree (make-spatial-tree kind :rectfun #'identity)))
    (dolist (r list)
      (insert r tree))
    (dolist (r (cdr list))
      (delete r tree))
    (let* ((results (search (car list) tree))
           (length (length results)))
      (is (= (length results) 1)
          "aargh: wrong amount of stuff (~D entries) in ~S"
          length tree))))

