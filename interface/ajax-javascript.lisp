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

;;;;; -----------------------------------------------------------------------------------
;;;;; Helper functions
;;;;; -----------------------------------------------------------------------------------

(defun value-or-nil (string)
  "Helper function for handling the value of HTML form elements."
  (if (string= "" string)
    nil
    string))
;; (value-or-nil "") ; >> NIL
;; (value-or-nil "Lorenzo Lotto") ; >> "Lorenzo Lotto"

;;;;; -----------------------------------------------------------------------------------
;;;;; Ajax
;;;;; -----------------------------------------------------------------------------------

;;;;; 1. Start the explorer by loading the artwork search form.
;;;;; -----------------------------------------------------------------------------------
(defun-ajax startexplorer () (*ajax-processor*)
  (reset) ; Reset the web interface.
  (art-network::artwork-search-form) ; Add the artwork search form
  nil)

;;;;; 2. Get the artwork data and initialize the integrative narrative network.
;;;;; -----------------------------------------------------------------------------------
(defun-ajax dogetartworkdata (artworkid artworkcreator artworktitle) (*ajax-processor*)
  ;; Function that searches the data about an artwork.
  (let ((id (value-or-nil artworkid))
        (creator (value-or-nil artworkcreator))
        (title (value-or-nil artworktitle)))
    ;; Only do something when the ID is given, or the creator AND title
    (when (or id (and creator title))
      (reset)
      ;; Get the artwork data.
      (art-network::explore-artwork (art-network:an-get-artwork-data :wikidata
                                                                     :id id
                                                                     :title title
                                                                     :creator creator))
      nil)))

;;;;; -----------------------------------------------------------------------------------
;;;;; Javascript
;;;;; -----------------------------------------------------------------------------------

(define-js 'submit-search "
    function submitSearch() {

     var varartworkId = document.getElementById('wikidataid').value;
     var varartworkCreator = document.getElementById('artworkcreator').value;
     var varartworkTitle = document.getElementById('artworktitle').value;

     ajax_dogetartworkdata(varartworkId, varartworkCreator, varartworkTitle);
     }")
