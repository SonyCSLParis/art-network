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

(defgeneric pose-artwork-question (art-network artwork-node-id wh-topic id))

;; WHO?
(defmethod pose-artwork-question ((inn art-network)
                                  (source-id integer)
                                  (wh-topic (eql 'creator))
                                  (id t))
  (declare (ignore wh-topic))
  (let ((target-id (inn-add-node inn (make-narrative-question
                                      :label "Creator?"
                                      :description "Identify the creator of an artwork."
                                      :posed-by 'expert-system
                                      :irl-programs 
                                      (list `((identify-creator ?creator ?artwork-data)
                                              (bind artwork-data ?artwork-data ,id)))))))
    (inn-add-edge inn source-id target-id)
    target-id))