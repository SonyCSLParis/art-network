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

(in-package :web-interface)

(defun show-start-screen ()
  (vis-destroy-network)
  (clear-page)
  (add-element `((a :href "javascript:ajax_startexplorer();")
                 ((img :src "images/welcome.jpg"
                       :width "810"
                       :height "640")))))
;; (show-start-screen)

(in-package :art-network)

(defun launch-artwork-explorer (&key (open-web-interface nil))
  "Launch the application"
  (if open-web-interface
    (sys:open-url "http://localhost:8000/"))
  (wi::show-start-screen))
;; Set :open-web-interface only to T if you haven't got the web interface opened yet:
;; (launch-artwork-explorer :open-web-interface t)
