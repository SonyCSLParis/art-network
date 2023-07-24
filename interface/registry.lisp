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

(activate-vis-js)

(setf ;; Hide the reset button:
      *no-reset-button* t
      ;; Registry of images, media, fonts, and so on.
      *dispatch-table*
      (append *dispatch-table*
              (list (create-folder-dispatcher-and-handler
                     "/images/"
                     (babel-pathname :directory '("grammars" "art-network" "imgs"))))))

(export '(make-place-holder-image))

(defun make-place-holder-image (width height &key (class "left") alt)
  `((img :src ,(format nil "https://via.placeholder.com/~ax~a.png" width height)
         :alt ,(or alt "")
         :class ,class)))