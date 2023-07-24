
;; SPARQL queries for Zeri and Lode
;; ----------------------------------
;; photo archive of the art historian Federico Zeri. Website: http://data.fondazionezeri.unibo.it
;; pictures of artworks of the XVI Century are expressed in LOD. 
;; sparql endpoint from interface: http://data.fondazionezeri.unibo.it/query/ 
;; sparql endpint to be called remotely: http://data.fondazionezeri.unibo.it/sparql/
;; data modelling description: https://figshare.com/articles/journal_contribution/Mapping_OA_Entry_to_RDF/3175057 


; endpoint:
;(defparameter *zeri-sparql-endpoint* "http://data.fondazionezeri.unibo.it/sparql/")


(defun sparql-query-zeri-artwork-subject (art-id) ;  &optional (kg :zeri))
  "retrieve artwork's subject"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

             SELECT DISTINCT ?subject ?subjectLabel WHERE{

                            <~a> fabio:hasSubjectTerm ?subject. 
                            ?subject rdfs:label ?subjectLabel.
                     
               } " art-id))


;(sparql-query-zeri-artwork-subject "https://w3id.org/zericatalog/artwork/47929")


(defun sparql-query-zeri-artwork-same-subject (art-id) ;  &optional (kg :zeri))
  "retrieve artwork's subject"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

             SELECT DISTINCT ?art2 ?art2Label WHERE{

                            <~a> fabio:hasSubjectTerm ?subject. 
                            ?art2 a fabio:ArtisticWork\; fabio:hasSubjectTerm ?subject\; rdfs:label ?art2Label. 
                             FILTER (?art2 != <~a>)
                             FILTER \(lang\(?art2Label\) = 'it'\)
                     
               } " art-id art-id))


;(sparql-query-zeri-artwork-same-subject "https://w3id.org/zericatalog/artwork/47929")

(defun sparql-query-zeri-artwork-type (art-id) ;  &optional (kg :zeri))
  "retrieve artwork's material"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

             SELECT DISTINCT ?type ?typeLabel WHERE{

                            <~a> crm:P2_has_type ?type.
                            ?type rdfs:label ?typeLabel.
                             FILTER \(lang\(?typeLabel\) = 'en'\)
               } " art-id))


;(sparql-query-zeri-artwork-type "https://w3id.org/zericatalog/artwork/47929")




(defun sparql-query-zeri-material (art-id) ;  &optional (kg :zeri))
  "retrieve artwork's material"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

             SELECT DISTINCT ?material ?label WHERE  {
                  <~a> fabio:hasManifestation / crm:P45_consists_of ?material.
                  ?material rdfs:label ?label.
                 FILTER \(lang\(?label\) = 'en'\)

               } " art-id))


;(sparql-query-zeri-material "https://w3id.org/zericatalog/artwork/47929")


