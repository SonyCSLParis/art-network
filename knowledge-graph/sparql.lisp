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

;; --------------------------------------------------------------------------------------------------------
;; SPARQL is the standard query language and protocol for Linked Open Data  
;; on the web or for RDF triplestores: https://www.ontotext.com/knowledgehub/fundamentals/what-is-sparql/
;; 
;; Learn more about how SPARQL works here:
;; https://www.w3.org/TR/rdf-sparql-query/
;; --------------------------------------------------------------------------------------------------------

(export '(sparql-query-artwork))

;; FORMAT templates for querying information from knowledge graphs.
(defun sparql-query-artwork (title creator &optional (language "en") (kg :wikidata))
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  ;; The SPARQL query template, with open slots for title, creator and language.
  (format nil 
  "SELECT ?uri
   WHERE { 
   ?uri rdfs:label ~s@~a.
   ?uri rdfs:label ?itemLabel.
   ?uri p:P170 ?statement.
   ?statement ps:P170 ?creator.
   ?creator rdfs:label ~s@~a.
 } limit 1"
  title language creator language))
;; (sparql-query-artwork "Venus and Cupid" "Lorenzo Lotto")

(defun sparql-query-depicts (id &optional (language "en") (kg :wikidata))
  (declare (ignorable kg))
  (format nil 
          "SELECT DISTINCT ?label ?identifier
           WHERE { wd:~a p:P180 ?statement .
          ?statement ps:P180 ?identifier .
         SERVICE wikibase:label { bd:serviceParam wikibase:language \"~a\". 
                                ?identifier rdfs:label ?label.
                                }
        }" id language))
;; (wikidata-sparql-query (sparql-query-depicts "Q4009580"))

(defun sparql-query-details (q-id &optional (kg :wikidata))
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  ;; SPARQL query to retrieve triples having as subject the input wikidata entity.
  (format nil 
  "SELECT DISTINCT ?rel ?realpropertyLabel ?obj ?objLabel WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en' . }
  wd:~a ?rel ?obj.
  ?realproperty wikibase:directClaim ?rel. 

  }"
  q-id))



;; SPARQL query to retrieve all the artworks of the same movement with the same subjects. Input: string of subjects with wd: prefix, country, (wd id); start date, end date (integers)

;;; (defparameter *artworks-period-subjects* "SELECT DISTINCT ?artwork ?startDate ?artworkLabel ?image WHERE {
;;;                                             SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
;;;                                             ?artwork wdt:P180 ~a. # subjects
;;;                                             ?artwork wdt:P170 / wdt:P937 / wdt:P17 wd:~a. # made by an artist who worked in country
;;;                                             OPTIONAL { ?artwork wdt:P571 ?startDate. }
;;;                                             OPTIONAL { ?artwork wdt:P18 ?image. }
;;;                                             FILTER (year(?startDate) >= ~a && year(?startDate) <= ~a) # In the High Renaissance period
;;;                                             }

;;;                                             ORDER BY ?startDate ")

; example with all the artworks depicting Venus and Cupid from the high renaissance in Italy 
;(format nil *artworks-period-subjects* "wd:Q47652, wd:Q5011" "Q38" "1494" "1527")


; SPARQL queries for Iconology Dataset
; ----------------------------------------------------------
; endpoint:
(defparameter *icon-sparql-endpoint* "https://projects.dharc.unibo.it/icondataset/sparql")


; query to search an arwork filtering by title and string labels

;;; (defparameter *art-icon-id* "PREFIX icon: <https://w3id.org/icon/ontology/> 
;;;     PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

;;;         SELECT DISTINCT ?artwork WHERE {
;;;         # VALUES ?rel {icon:hasCharacter icon:hasEvent icon:hasNamedObject icon:hasPlace icon:hasPersonification icon:hasSymbol}

;;;         ?obj crm:P65_shows_visual_item ?artwork; ^crm:P108_has_produced / crm:P14_carried_out_by ?artist.
;;;         ?artist rdfs:label ?artistLabel. 
;;;         ?obj crm:P102_has_title / rdfs:label ?titleLabel. 
;;;         FILTER(regex(str(?artistLabel), ('~a'), "i"))
;;;         FILTER(regex(str(?titleLabel), ('~a'), "i"))
;;;         } LIMIT 10 ")



; query to retrieve level 1 subjects, given an artwork ID

;;; (defparameter *art-icon-level1* 
;;;   (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
;;;         PREFIX d: <https://w3id.org/icon/data/> 
;;;     PREFIX icon: <https://w3id.org/icon/ontology/> 
;;;     PREFIX sim: <https://w3id.org/simulation/ontology/>
;;;     PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
;;;     PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

;;;     SELECT DISTINCT ?lev1 ?lev1Label WHERE {
;;;     ?rec icon:aboutWorkOfArt d:~a.   
;;;     {?rec icon:recognizedArtisticMotif ?am} UNION {?rec icon:recognizedComposition ?comp. ?comp icon:hasPart ?am}
;;;     ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1.
;;;     ?lev1 rdfs:label ?lev1Label. 
;;;                 
;;;     }")


; query to retrieve level 2 subjects composed by a level 1 subject, given an artwork ID

;;; (defparameter *art-icon-level2-level1* "    PREFIX d: <https://w3id.org/icon/data/> 
;;;     PREFIX icon: <https://w3id.org/icon/ontology/> 
;;;     PREFIX sim: <https://w3id.org/simulation/ontology/>
;;;     PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
;;;     PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

;;;     SELECT DISTINCT ?lev2 ?lev2Label (group_concat(distinct ?lev1Label; separator=', ') as ?subj1) WHERE {  
;;;     VALUES ?rel {icon:hasCharacter icon:hasEvent icon:hasNamedObject icon:hasPlace icon:hasPersonification icon:hasSymbol crm:P138_represents}
;;;     ?rec icon:aboutWorkOfArt d:~a; 
;;;         icon:refersToArtisticMotif ?l1;
;;;         icon:recognizedImage ?img. 
;;;         ?img ?rel ?lev2. 
;;;     ?lev2 rdfs:label ?lev2Label.  
;;;     {?l1 icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1.
;;;     } 
;;;     UNION 
;;;     {?l1 icon:hasPart ?am.
;;;     ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1. }

;;;     ?lev1 rdfs:label ?lev1Label. 

;;;                 
;;;     } GROUP BY ?lev2 ?lev2Label ")

; query to retrieve all the symbols in an artwork along with their sources

;;; (defparameter *art-icon-symbols-sources* "PREFIX icon: <https://w3id.org/icon/ontology/> 
;;;     PREFIX d: <https://w3id.org/icon/data/> 
;;;     PREFIX icon: <https://w3id.org/icon/ontology/> 
;;;     PREFIX sim: <https://w3id.org/simulation/ontology/>
;;;     PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
;;;     PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
;;;     PREFIX cito: <http://purl.org/spar/cito/>

;;;     SELECT DISTINCT ?meaning ?label WHERE {  
;;;         ?iconol icon:aboutWorkOfArt d:~a;
;;;         icon:recognizedIntrinsicMeaning ?intrinsic.
;;;         ?intrinsic icon:recognizedCulturalPhenomenon | icon:recognizedConceptualObject ?meaning.
;;;         ?meaning rdfs:label ?label. 
;;;   
;;;     }")


#|
(defparameter *test* nil)



|#

