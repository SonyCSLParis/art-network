(in-package :art-network)


;; Simple formatting
;; --------------------------------------------------------------------------------------------------------------------

(defun format-node (id label)
  "format the node name and label as a string to be added to the graph"
  (format nil "{id: '~a', label: '~a'}" id label))

(defun format-edge (input output label)
  "format the edge details and label as a string to be added to the graph"
  (format nil "{from: '~a', to: '~a', label: '~a'}" input output label))

(defun add-formatted-nodes (my_list)
   (loop for line in my_list do (wi::vis-add-node (format-node (first line) (second line)))))

(defun add-formatted-edges (my_list)
  "add edges between already existing nodes"
   (loop for line in my_list do (wi::vis-add-edge (format-edge (first line) (second line)(third line)))))


(defun add-info-to-node (node my_list)
  "Add edges and nodes detail having as subject the id of a given node already in the graph. Loop over a list of lists having as structure: relation-id relation-label node-id node-label"
              (loop for line in my_list do (wi::vis-add-node (format-node (third line) (fourth line))))
              (loop for line in my_list do (wi::vis-add-edge (format-edge node (third line)(second line)))))


(defun format-info-to-node (node my_list)
  "Add edges and nodes detail having as subject the id of a given node already in the graph. Loop over a list of lists having as structure: relation-id relation-label node-id node-label"
              (loop for line in my_list do (format-node (third line) (fourth line)))
              (loop for line in my_list do (format-edge node (third line)(second line))))


;; Formatting with a color according to the source (manual retrieval, from query, from textrazor etc.) 
;; --------------------------------------------------------------------------------------------------------------------

(defun format-node-color (id label source)
  "format the node name, color and label as a string to be added to the graph. Color according to the source"
  (cond ((string= source "textrazor") (setf *color* "#C2FABC"))
   ((string= source "kb-query") (setf *color* "#FFF7AD"))
   ((string= source "kb-manual") (setf *color* "#ffbf47")) ;manually found in kb
   ((string= source "kb-text") (setf *color* "#feb2f8")) ; found in kb and then cited by the text
   ((string= source "fcg") (setf *color* "#8fbdff")) ; semantic frames
   ((string= source "wn") (setf *color*"#7cfbe3")) ;wordnet
   ((string= source "manual") (setf *color* "#FB7E81")))
  (format nil "{id: '~a', label: '~a', color: '~a'}" id label *color*))

(defun format-edge-color (input output label source)
  "format the edge color and label as a string to be added to the graph. Color according to the source"
  (cond ((string= source "textrazor") (setf *color* "#C2FABC"))
   ((string= source "kb-query") (setf *color* "#FFF7AD"))
   ((string= source "kb-manual") (setf *color* "#ffbf47")) ;manually found in kb
   ((string= source "kb-text") (setf *color* "#feb2f8")) ; found in kb and then cited by the text
   ((string= source "fcg") (setf *color* "#8fbdff")) ; semantic frame
   ((string= source "wn") (setf *color*"#7cfbe3")) ;wordnet
   ((string= source "manual") (setf *color* "#FB7E81")))
  (format nil "{from: '~a', to: '~a', label: '~a', color: '~a'}" input output label *color*))

(defun add-formatted-nodes-color (my_list)
   (loop for line in my_list do (wi::vis-add-node (format-node-color (first line) (second line)(third line)))))

(defun add-formatted-edges-color (my_list)
  "add edges between already existing nodes"
   (loop for line in my_list do (wi::vis-add-edge (format-edge-color (first line) (second line)(third line)(fourth line)))))

(defun add-info-to-node-color (node my_list source)
  "Add edges and nodes detail having as subject the id of a given node already in the graph. Loop over a list of lists having as structure: relation-id relation-label node-id node-label. Color according to the source"
              (loop for line in my_list do (wi::vis-add-node (format-node-color (third line) (fourth line) source)))
              (loop for line in my_list do (wi::vis-add-edge (format-edge-color node (third line)(second line) source))))


;; Store results in csv
;; --------------------------------------------------------------------------------------------------------------------
;; idea: progressively store all the results obtained by the filtering in a csv file, in the format: 
;; "subj" "subjLabel" "rel" "relLabel "obj" "objLabel" "color"
;; NB: this works only for the bulk additions by the same source. For initial nodes and edges with different colors we should try something else. 

;; first: add the subj and subjLabel to the beginning of the list: 
; (setf d-list (mapcar #'(lambda (l) (cons "q1" (cons "epit" l))) d-list)) 
(defun add-subject (my-list id label)
  (mapcar #'(lambda (l) (cons id (cons label l))) my-list))

;; second: append the rows to a csv file, without overwriting it. Pay attention to not insert lines twice

;; (read-csv #P"file.csv") reads a csv as a list of lists


