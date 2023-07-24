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

;; This file is currently obsolete.

(in-package :art-network)

;; This file concerns the icon-ontology developed by Sofia Baroncini.
;; Currently this function is not used; instead we query the knowledge base 
;; and ontology directly through a python interface.
(defun parse-ontology (pathname)
  (with-open-file (ontology pathname)
    (xmls::parse ontology)))
;; (parse-ontology *owl-ontology*)