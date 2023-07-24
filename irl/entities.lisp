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

;;; -----------------------------------------------------------------------------
;;; Basic Classes
;;; -----------------------------------------------------------------------------

(defclass art-network-entity (entity)
  ((node-id :type integer
            :accessor node-id
            :initform 0
            :initarg :node-id)
   (label :type string
          :initform (symbol-name (gensym "?"))
          :initarg :label
          :accessor label))
  (:documentation "Basic entity class used in the art-network case study."))

(defclass entity-set (entity)
  ((entities :type list :initarg :entities :accessor entities)))

(defmethod equal-entity ((set1 entity-set) (set2 entity-set))
  (permutation-of? (entities set1) (entities set2) :test #'equal-entity))

(defmethod make-html-for-entity-details ((entity art-network-entity) 
                                         &key)
  `(((div :class "entity-detail" :style "text-align:left")
     ((table)
      ((td)
       "Node ID:")
      ((td) ,(node-id entity))))
    ((div :class "entity-detail" :style "text-align:left")
     ((table)
      ((td) "Label:")
      ((td) ,(label entity))))))
;; (add-element (make-html (make-instance 'wikidata-property :node-id 1 :label "test")))

;;; -----------------------------------------------------------------------------
;;; Wikidata
;;; -----------------------------------------------------------------------------

(defclass wikidata-entity (art-network-entity)
  ((instance-of :initform nil
                :initarg :instance-of
                :accessor instance-of))
  (:documentation "An entity from Wikidata."))

(defclass wikidata-property (wikidata-entity)
  ()
  (:documentation "A property from Wikidata."))

(defclass wikidata-concept (art-network-entity)
  ((subclass-of :initform nil
                :initarg :subclass-of
                :accessor subclass-of))
  (:documentation "An entity from Wikidata."))