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

;;; -------------------------------------------------------------
;;; Base Class
;;; -------------------------------------------------------------

(defclass discourse-entity (entity)
  ()
  (:documentation 
   "Base class for all things related to the discourse model"))

;;; -------------------------------------------------------------
;;; Situation Model
;;; -------------------------------------------------------------

(defclass situation-model (discourse-entity)
  ((entities
    :initarg :entities
    :accessor entities
    :initform (make-instance 'entity-set))
   (parent :initarg :parent
           :initform nil
           :accessor parent
           :documentation "Holds the ID of the parent situation model.")
   (children :initarg :children
             :initform nil
             :accessor children
             :documentation "List of IDs of the children situation models."))
  (:documentation "The model of a 'situation' represents which entities are
                   currently accessible in the network."))

;;; -------------------------------------------------------------
;;; Discourse Model
;;; -------------------------------------------------------------

(defclass discourse-model (discourse-entity)
  ((current-situation :type 'situation-model 
                      :initform (make-instance 'situation-model)
                      :initarg :current-situation
                      :accessor current-situation
                      :documentation "Holds the current situation model.")
   (situation-history :initform nil
                      :initarg :situation-history
                      :accessor situation-history
                      :documentation "List of previous situation models."))
  (:documentation "Model of the discourse."))
