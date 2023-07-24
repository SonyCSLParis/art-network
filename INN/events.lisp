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

(defmethod inn-double-click (selection
                             (inn art-network))

  (call-next-method))
;; (draw-inn-network-in-vis-js (get-current-inn))

(defmethod inn-right-click ((inn art-network))
  (add-element `((div :class "popup"
                      :onclick "javascript:myFunction();")
                 "Click me! <span class=\"popuptext\" id=\"myPopup\">Popup text...</span>"))
  (call-next-method))