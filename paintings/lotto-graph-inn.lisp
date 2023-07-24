;; (ql:quickload :art-network)

;; This file illustrates how Sofia's network could be constructed (manually) using
;; the new INN package.

(in-package :art-network)

;; Recreating Sofia's graph using code from INN (Integrative Narrative Networks):
;; ------------------------------------------------------------------------------

(defmethod get-node-shape ((type (eql :concept)))
  "rectangle")

(defmethod get-node-shape ((type (eql :concept)))
  "purple")




(defmethod get-node-shape ((type (eql :artwork)))
  "circularImage")

(defmethod get-node-color ((type (eql :textrazor)))
  "#C2FABC")

(defmethod get-node-color ((type (eql :kb-query)))
  "#FFF7AD")

(defmethod get-node-color ((type (eql :kb-manual)))
  "#ffbf47")

(defmethod get-node-color ((type (eql :kb-text)))
  "#feb2f8")

(defmethod get-node-color ((type (eql :fcg)))
  "#8fbdff")

(defmethod get-node-color ((type (eql :wn)))
  "#7cfbe3")

(defmethod get-node-color ((type (eql :manual)))
  "#FB7E81")

; step 1: artwork metadata
; _____________________________________________________________________________________________________________

(defparameter *lotto-graph* nil)

(let ((art-network (setf *lotto-graph* (make-instance 'art-network))))
  ;; Draw the (empty) network:
  (draw-inn-network-in-vis-js art-network)
  ;; Add and draw the nodes:
  (inn-add-nodes art-network 
                 (list (make-inn-image :description "Lorenzo Lotto, Venus and Cupid"
                                       :type :artwork
                                       :url "https://images.metmuseum.org/CRDImages/ep/original/DP-25975-001.jpg") ; 0
                       (make-answered-narrative-question :label "title") ; 1
                       (make-inn-node :label "Venus and Cupid"
                                      :type :manual) ; 2
                       (make-answered-narrative-question :label "Creator") ; 3
                       (make-inn-node :label "Lorenzo Lotto" :type :kb-query) ; 4
                       (make-answered-narrative-question :label "Medium") ; 5
                       (make-inn-node :label "oil painting" :type :textrazor) ; 6
                       (make-answered-narrative-question :label "instance of") ; 7
                       (make-inn-node :label "painting" :type :kb-query) ; 8
                       (make-answered-narrative-question :label "gifted by") ; 9
                       (make-inn-node :label "Charles Bierer Wrightsman" :type :textrazor) ; 10
                       (make-inn-node :label "Venice" :type :textrazor)
                       (make-inn-node :label "Loreto" :type :textrazor)))
  ;; Add and draw the edges:
  (inn-add-edges art-network '((0 1) (1 2)
                               (0 3) (3 4)
                               (0 5) (5 6)
                               (0 7) (7 8)
                               (0 9) (9 10))))
  
; step 1: painting details
; _____________________________________________________________________________________________________________
(let ((art-network (get-current-inn)))
  ;; Depicts
  (let ((question-node-id (inn-add-node art-network (make-answered-narrative-question :label "depicts")))
        (target-ids (loop for detail in '("nudity" "Hedera" "Venus" "Venus" "snake" "urolagnia" "conch"
                                          "aspic viper" "woman" "smile" "nude" "rose" "grass" "fascia"
                                          "standing" "veil" "ribbon" "swagger stick" "diadem" "switch"
                                          "trunk" "areola" "petal" "seashell" "censer" "half reclining"
                                          "bird's wing" "earring" "bracelet" "laurel wreath" "urination"
                                          "pubic hair removal" "drapery" "navel" "nude")
                          collect (inn-add-node art-network (make-inn-node :label detail :type :kb-query)))))
    (inn-add-edge art-network 0 question-node-id)
    (loop for target-id in target-ids
          do (inn-add-edge art-network question-node-id target-id))))