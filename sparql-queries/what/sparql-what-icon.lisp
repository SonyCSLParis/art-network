
(in-package :art-network)

;; --------------------------------------------------------------------------------------------------------
;; SPARQL is the standard query language and protocol for Linked Open Data  
;; on the web or for RDF triplestores: https://www.ontotext.com/knowledgehub/fundamentals/what-is-sparql/
;; 
;; Learn more about how SPARQL works here:
;; https://www.w3.org/TR/rdf-sparql-query/
;; --------------------------------------------------------------------------------------------------------


; SPARQL queries for Iconology Dataset
; the Iconology dataset was created by Sofia Baroncini as part of her PhD. 
; It contains the interpretations by the art historian Erwin Panofsky made about ca. 400 artworks in 3 of his books. 
; An introduction and exploratory data analysis are available here: 
; https://iconology-dataset.streamlit.app 
; ----------------------------------------------------------
; endpoint:
(defparameter *icon-sparql-endpoint* "https://projects.dharc.unibo.it/icondataset/sparql")


;; what is depicted? 

;; level 1
(defun sparql-what-icon-level1 (art-id   &optional (kg :icon-dataset))
 "query to retrieve level 1 subjects, given an artwork ID"

  (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
        PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

    SELECT DISTINCT ?lev1 ?lev1Label WHERE {
    ?rec icon:aboutWorkOfArt d:~a.   
    {?rec icon:recognizedArtisticMotif ?am} UNION {?rec icon:recognizedComposition ?comp. ?comp icon:hasPart ?am}
    ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1.
    ?lev1 rdfs:label ?lev1Label. 
                
    }" art-id))
; (sparql-what-icon-level1 "ART1002test")

(defun sparql-what-icon-level1-structure (art-id   &optional (kg :icon-dataset))
 "query to retrieve the possible compositional structure of level 1 subjects, given an artwork ID"

  (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

    SELECT DISTINCT ?lev1 ?lev1Label ?comp ?structure ?structureLabel WHERE {
    ?rec icon:aboutWorkOfArt d:~a.   
    ?rec icon:recognizedComposition ?comp. ?comp icon:hasPart ?am\; icon:hasCompositionalStructure ?structure.
    ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1.
    ?lev1 rdfs:label ?lev1Label.     
    ?structure rdfs:label ?structureLabel. 
    
          
                
    }" art-id))
; (sparql-what-icon-level1-structure "ART1160")


(defun sparql-what-icon-level1-quality (art-id   &optional (kg :icon-dataset))
 "query to retrieve possible further details of level 1 subjects, given an artwork ID"

  (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

    SELECT DISTINCT ?lev1Label ?qualityLabel WHERE {
    ?rec icon:aboutWorkOfArt d:~a.   
    {?rec icon:recognizedArtisticMotif ?am} UNION {?rec icon:recognizedComposition ?comp. ?comp icon:hasPart ?am}
    ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1.
    ?lev1 rdfs:label ?lev1Label. 
    ?am dul:hasQuality ?quality. 
    ?quality rdfs:label ?qualityLabel.             
                
    }" art-id))
; (sparql-what-icon-level1-structure "ART1002test")


; query to retrieve level 2 subjects composed by a level 1 subject, given an artwork ID
(defun sparql-what-icon-level2-and-1 (art-id   &optional (kg :icon-dataset))
  (format nil "PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

    SELECT DISTINCT ?lev2 ?lev2Label \(group_concat\(distinct ?lev1Label; separator=', '\) as ?subj1\) WHERE {  
    VALUES ?rel {icon:hasCharacter icon:hasEvent icon:hasNamedObject icon:hasPlace icon:hasPersonification icon:hasSymbol crm:P138_represents}
    ?rec icon:aboutWorkOfArt d:~a; 
        icon:refersToArtisticMotif ?l1\;
        icon:recognizedImage ?img. 
        ?img ?rel ?lev2. 
    ?lev2 rdfs:label ?lev2Label.  
    {?l1 icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1.
    } 
    UNION 
    {?l1 icon:hasPart ?am.
    ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1. }
                
    } GROUP BY ?lev2 ?lev2Label" art-id))

; (sparql-what-icon-level2-and-1 "ART1002test")




(defun sparql-what-icon-level3 (art-id  &optional (kg :icon-dataset))
(format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX cito: <http://purl.org/spar/cito/>

    SELECT DISTINCT ?meaning ?label WHERE {  
    ?iconol icon:aboutWorkOfArt d:~a;
        icon:recognizedIntrinsicMeaning ?intrinsic.
        ?intrinsic icon:recognizedCulturalPhenomenon | icon:recognizedConceptualObject ?meaning.
        ?meaning rdfs:label ?label.
  
    }" art-id))

;(sparql-what-icon-level3 "ART1002test")

(defun sparql-what-icon-level3-refer-to (art-id  &optional (kg :icon-dataset))
  (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX cito: <http://purl.org/spar/cito/>

    SELECT DISTINCT ?meaningLabel ?label WHERE {  
      VALUES ?rel {icon:hasCharacter icon:hasEvent icon:hasNamedObject icon:hasPlace icon:hasPersonification icon:hasSymbol crm:P138_represents}
   
      ?iconol icon:aboutWorkOfArt d:~a\;
        icon:recognizedIntrinsicMeaning ?intrinsic.
        ?intrinsic icon:recognizedCulturalPhenomenon | icon:recognizedConceptualObject ?meaning.
        ?meaning rdfs:label ?meaningLabel. 

      {?intrinsic icon:hasComposition ?comp. ?comp icon:hasPart ?am.
      ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1. ?lev1 rdfs:label ?label} 
      UNION 

      {?intrinsic icon:hasArtisticMotif ?am. ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1. ?lev1 rdfs:label ?label}
      UNION
      {?intrinsic icon:hasImage ?image. ?image ?rel ?lev2. ?lev2 rdfs:label ?label.}
  
    }" art-id))

;(sparql-what-icon-level3-refer-to "ART1002test")


;; sources of the recognition

(defun sparql-what-icon-recognition-sources (art-id  &optional (kg :icon-dataset))
  "query to retrieve all the sources cited when performing a subject recognition"
  (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX cito: <http://purl.org/spar/cito/>

    SELECT DISTINCT ?label ?evid ?evidLabel WHERE {  
      VALUES ?rel {icon:hasCharacter icon:hasEvent icon:hasNamedObject icon:hasPlace icon:hasPersonification icon:hasSymbol crm:P138_represents}
      ?rec icon:aboutWorkOfArt d:~a\; 
        cito:citesAsEvidence ?evid.
      ?evid rdfs:label ?evidLabel. 

      {?rec icon:recognizedComposition ?comp. ?comp icon:hasPart ?am.
      ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1. ?lev1 rdfs:label ?label} 
      UNION 

      {?rec icon:recognizedArtisticMotif ?am. ?am icon:hasFactualMeaning | icon:hasExpressionalMeaning | crm:P138_represents ?lev1. ?lev1 rdfs:label ?label}
      UNION
      {?rec icon:recognizedImage ?image. ?image ?rel ?lev2. ?lev2 rdfs:label ?label.}
      UNION
      {?rec icon:recognizedInvenzione ?inv. ?inv rdfs:label ?label.}
  
    }" art-id))

; (sparql-what-icon-recognition-sources "ART1002test")



(defun sparql-what-icon-symbols-sources (art-id  &optional (kg :icon-dataset))
  "query to retrieve all the symbols in an artwork along with their sources"

  (format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX d: <https://w3id.org/icon/data/> 
    PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX sim: <https://w3id.org/simulation/ontology/>
    PREFIX dul: <http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX cito: <http://purl.org/spar/cito/>

    SELECT DISTINCT ?lev2 ?lev2Label ?evid ?evidLabel WHERE {  
    ?rec icon:aboutWorkOfArt d:~a\; 
        icon:recognizedImage ?img. 
        ?img icon:hasSymbol ?lev2. 
    ?lev2 rdfs:label ?lev2Label\; cito:citesAsEvidence ?evid. 
    ?evid rdfs:label ?evidLabel. 
    FILTER \(?evidLabel != ''\)
  
    }" art-id))

;(sparql-what-icon-symbols-sources "ART1002test")
