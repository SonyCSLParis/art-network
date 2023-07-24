(in-package :art-network)

; who is the author?
(defun sparql-who-icon-author (art-id &optional (kg :icon-dataset))
  "SPARQL query to retrieve the author"
  (format nil 
   "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX d: <https://w3id.org/icon/ontology/data/>

        SELECT DISTINCT ?artist ?artistLabel WHERE {
       
        ?obj crm:P65_shows_visual_item d:~a\; ^crm:P108_has_produced / crm:P14_carried_out_by ?artist.
        ?artist rdfs:label ?artistLabel. 
    }"
  art-id))
; (sparql-who-icon-author "ART1002test")

; who is the patron?
(defun sparql-who-icon-patron (art-id &optional (kg :icon-dataset))
  "SPARQL query to retrieve the patron"
  (format nil 
   "PREFIX icon: <https://w3id.org/icon/ontology/> 
    PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
    PREFIX d: <https://w3id.org/icon/ontology/data/>

        SELECT DISTINCT ?patron ?patronLabel WHERE {
       
        ?obj crm:P65_shows_visual_item d:~a\; ^crm:P108_has_produced / crm:P17_was_motivated_by ?patron.
        ?patron rdfs:label ?patronLabel. 
    }"
  art-id))
; (sparql-who-icon-patron "ART1002test") - nb: very few artworks have this specification

