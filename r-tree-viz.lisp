(in-package :clim-user)

;;;; R-Tree Visualization Toy

;; For best results, use a McCLIM newer than Nov.11, 2004  :)

(define-presentation-type r-tree-node ())
(define-presentation-type rectangle   ())

(define-application-frame r-tree-viz ()
  ((tree :initarg :tree :reader tree)
   (scale :initarg :scale :initform 200 :accessor scale)
   (expanded-nodes :initform (make-hash-table) :reader expanded-nodes))
  (:panes (hierarchy-pane (make-pane 'application-pane
                                     :display-time t
                                     :end-of-page-action :allow
                                     :end-of-line-action :allow
                                     :text-style (make-text-style :sans-serif :roman :normal)
                                     :display-function 'print-tree-hierarchy))
          (inspect        (make-pane 'application-pane :display-time nil
                                     :end-of-page-action :allow
                                     :end-of-line-action :allow))
          (viz            (make-pane 'application-pane :display-time t                                     
                                     :display-function 'draw-layout))
          (zoom-in        (make-pane 'push-button :label "Zoom In"
                                     :activate-callback 'zoom-in))
          (zoom-out       (make-pane 'push-button :label "Zoom Out"
                                     :activate-callback 'zoom-out)))
  (:command-table (r-tree-viz))
  (:pointer-documentation t)  
  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (labelling (:label "Hierarchy")
             (scrolling (:scroll-bars :vertical)
               hierarchy-pane))
           (make-pane 'clim-extensions:box-adjuster-gadget)
           (labelling (:label "Layout" :width +fill+)
             (vertically ()
               (scrolling (:suggested-width 500 :suggested-height 500)
                 viz)
               (horizontally () zoom-in zoom-out))))
         (make-pane 'clim-extensions:box-adjuster-gadget)
         (labelling (:label "Details")
           (scrolling (:suggested-width 600)
             inspect))))))

;;; Display Code

(defun print-tree-node (frame pane node &key (indent 0))
  (indenting-output (pane indent)
    (etypecase node
      (r-trees::r-tree-node
       (with-output-as-presentation (pane node 'r-tree-node)
         (format pane "~A (~A children)~%" (type-of node) (length (r-trees::children node)))))
      (r-trees::rectangle
       (with-output-as-presentation (pane node 'rectangle)
         (multiple-value-call #'format pane "Rectangle (~1,2F,~1,2F)-(~1,2F,~1,2F)~%"
                              (rect* node)))))
    (when (gethash node (expanded-nodes frame))
      (dolist (child (r-trees::children node))
        (print-tree-node frame pane child :indent (+ indent 16))))))

(defun print-tree-hierarchy (frame pane)
  (print-tree-node frame pane (r-trees::root-node (tree frame))))

(defun rect* (rectangle)
  (values
   (first (r-trees::lows rectangle)) (second (r-trees::lows rectangle))
   (first (r-trees::highs rectangle)) (second (r-trees::highs rectangle))))
   
(defun draw-layout (frame pane &optional (node (tree frame)))
  (etypecase node
    (r-trees::r-tree
     (with-room-for-graphics (pane :first-quadrant nil)     
       (with-scaling (pane (scale frame))
         (draw-layout frame pane (r-trees::root-node node))))
     (change-space-requirements pane               ;; FIXME: McCLIM should do this itself.
                                :width  (bounding-rectangle-width (stream-output-history pane))
                                :height (bounding-rectangle-height (stream-output-history pane))))
    (r-trees::r-tree-leaf-node
     (dolist (child (slot-value node 'r-trees::children))
       (draw-layout frame pane child))
     (when (slot-boundp node 'r-trees::mbr)
       (multiple-value-call #'draw-rectangle*
         pane (rect* (slot-value node 'r-trees::mbr))
         :ink +red+ :filled nil)))
    (r-trees::r-tree-node     
     (dolist (child (slot-value node 'r-trees::children))
       (draw-layout frame pane child))
     (when (slot-boundp node 'r-trees::mbr)
       (multiple-value-call #'draw-rectangle*
         pane (rect* (slot-value node 'r-trees::mbr))
         :ink +black+ :filled nil)))
    (r-trees::rectangle
     (with-output-as-presentation (pane node 'rectangle)
       (multiple-value-call #'draw-rectangle*
       pane (rect* node)
       :ink +blue+ :filled nil :line-dashes #(1 1))))))

;;; Callbacks

(defun zoom-in (pane)
  (declare (ignore pane))
  (setf (scale *application-frame*)
        (* 2 (scale *application-frame*)))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'viz) :force-p t))

(defun zoom-out (pane)
  (declare (ignore pane))
  (setf (scale *application-frame*)
        (/ (scale *application-frame*) 2))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'viz) :force-p t))

;;; Commands

(define-r-tree-viz-command (com-toggle-node :name "Toggle Expand Node")
    ((node 'r-tree-node :prompt :node :gesture :select))
  (if (gethash node (expanded-nodes *application-frame*))
      (remhash node (expanded-nodes *application-frame*))
      (setf (gethash node (expanded-nodes *application-frame*)) t))
  (setf (pane-needs-redisplay (get-frame-pane *application-frame* 'hierarchy-pane)) t))

(define-r-tree-viz-command (com-describe-node :name "Describe Node")
    ((node 'r-tree-node :prompt :node :gesture :describe))
  (describe node (get-frame-pane *application-frame* 'inspect)))

(define-r-tree-viz-command (com-describe-rectangle :name "Describe Rectangle")
    ((node 'rectangle :prompt :node :gesture :describe))
  (describe node (get-frame-pane *application-frame* 'inspect)))

;;; Foo

(defun inspect-r-tree (tree)
  (run-frame-top-level (make-application-frame 'clim-user::r-tree-viz :tree tree :pretty-name "R-Tree Visualizer")))
