;; Copyright Sony Computer Science Laboratory - Paris
;;           Sofia Baroncini
;;           Luc Steels
;;           Remi van Trijp

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :art-network)

(defun extract-artwork-node-information (artwork-data &key (language "en"))
  (assert (hash-table-p artwork-data))
  (let* ((wikidata-id (get-wikidata-entity-id artwork-data))
         (image (wikidata-property artwork-data :image))
         (label (get-entity-label artwork-data language))
         (description (get-entity-description artwork-data language))
         (depicts (an-get-artwork-depicts :wikidata
                                          :id wikidata-id)))
    (values wikidata-id image label description depicts)))

(defun ask-initial-artwork-questions (art-network artwork-node-id artwork-id)
  (loop for wh-topic in '(creator)
        collect (pose-artwork-question art-network artwork-node-id wh-topic artwork-id)))

(defun explore-artwork (artwork-data 
                        &key (knowledge-graph :wikidata) 
                        (language "en"))
  (let ((art-network (make-instance 'art-network)))
    ;; We draw the network in vis-js:
    (draw-inn-network-in-vis-js art-network
                                :id "artExplorerNetwork")
    (multiple-value-bind (wikidata-id image label description depicts)
        (extract-artwork-node-information artwork-data :language language)
      (let* ((artwork-node-id (inn-add-node art-network (make-artwork-node
                                                         :url image
                                                         :description description
                                                         :label label 
                                                         :attributes `((:class . artwork-data)
                                                                       (:wikidata-id . ,wikidata-id)
                                                                       (:wikidata . ,artwork-data)
                                                                       (:depicts . ,depicts)))))
             (question-ids (ask-initial-artwork-questions art-network artwork-node-id wikidata-id)))
        (dolist (question-id question-ids)
          (inn-add-edge art-network artwork-node-id question-id))
        art-network))))
; (setf *artwork-data* (an-get-artwork-data :wikidata :id "Q4009580"))
; (explore-artwork *artwork-data*)
