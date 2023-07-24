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

;; This file specifies the "API" for interfacing with a knowledge graph. 
;; All functions are prefixed with "an-" (short for art-network-).
;; ---------------------------------------------------------------------

(export '(an-get-artwork-data))

;; Obtaining data about an artwork from a knowledge graph or triplestore.
(defgeneric an-get-artwork-data (knowledge-graph &key id title creator language &allow-other-keys)
  (:documentation "Access or search for an artwork based on either its title AND creator."))

(defmethod an-get-artwork-data ((knowledge-graph t)
                                &key &allow-other-keys)
  (error (format nil "Please implement the method AN-GET-ARTWORK-DATA for knowledge graph ~a" knowledge-graph)))
;; (an-get-artwork-data :dbpedia)

(defgeneric an-get-artwork-depicts (knowledge-graph &key id language &allow-other-keys)
  (:documentation "Returns a list of labels and URIs for entities depicted in an artwork."))

(defmethod an-get-artwork-depicts ((knowledge-graph t)
                                &key &allow-other-keys)
  (error (format nil "Please implement the method AN-GET-ARTWORK-DEPICTS for knowledge graph ~a" knowledge-graph)))
;; (an-get-artwork-depicts :dbpedia)
