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

(setf cl-wikimedia::*user-agent* "firstname.surname@gmail.com")

(defun sparql-endpoint (knowledge-source)
  (case knowledge-source
    (wikidata *wikidata-sparql-endpoint*)
    (framester "http://etna.istc.cnr.it/framester2/sparql")
    (t
     (error (format nil "No sparql endpoint defined for ~a." knowledge-source)))))
;; (sparql-endpoint 'wikidata)
;; (sparql-endpoint 'framester)
;; (sparql-endpoint 'new-source)



#|
;; Obsolete, to be removed:
(defstruct artwork id catalog-entry title creator date medium description meta-language)

(defparameter *artwork* nil "A first case study.")

(setf *artwork*
      (make-artwork :id "Q4009580"
                    :catalog-entry "1986.138"
                    :title "Venus and Cupid"
                    :creator "Lorenzo Lotto"
                    :date "1520"
                    :medium "oil on canvas"
                    :meta-language "en"
                    :description (list "In his inimitable fashion, in this unique masterpiece Lorenzo Lotto takes one of the most popular subjects of Venetian painting and gives it a witty and humorous twist."
                                       "Naughty Cupid urinates on his mother through a laurel wreath - an act meant to symbolize fertility."
                                       "Because this is a marriage picture, it is inspired by ancient marriage poems."
                                       "It is possible that Venus’s features may be taken from the bride's."
                                       "The beautifully observed details relate to the goddess and marriage."
                                       "The ivy is symbolic of fidelity; brides wore a myrtle wreath; incense perfumes the bower; a snake warns against jealousy.")))
|#