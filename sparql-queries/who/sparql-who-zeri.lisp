
;; SPARQL queries for Zeri and Lode
;; ----------------------------------
;; photo archive of the art historian Federico Zeri. Website: http://data.fondazionezeri.unibo.it
;; pictures of artworks of the XVI Century are expressed in LOD. 
;; sparql endpoint from interface: http://data.fondazionezeri.unibo.it/query/ 
;; sparql endpint to be called remotely: http://data.fondazionezeri.unibo.it/sparql/
;; data modelling description: https://figshare.com/articles/journal_contribution/Mapping_OA_Entry_to_RDF/3175057 



(defun sparql-who-zeri-artist (zeri-id) ;  &optional (kg :icon))
  "retrieve artist and artist label form artwork id"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             PREFIX zeri-art: <https://w3id.org/zericatalog/artwork/>
             SELECT DISTINCT ?artist ?artistLabel WHERE {
                             zeri-art:~a a fabio:ArtisticWork \;
                             crm:P94i_was_created_by / crm:P14_carried_out_by ?artist. 
                             ?artist rdfs:label ?artistLabel.
    
              } " zeri-id))


;(sparql-query-zeri-artist "47929")


(defun sparql-who-zeri-artist-movement (artist-url) ;  &optional (kg :icon))
  "retrieve artist's movement from artist internal url"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

             SELECT DISTINCT ?school ?schoolLabel WHERE {
                 <~a> crm:P107i_is_current_or_former_member_of ?school.
                ?school rdfs:label ?schoolLabel.
    
              } " artist-url))


;(sparql-query-zeri-artist-movement "https://w3id.org/zericatalog/person/6225/lotto-lorenzo")


(defun sparql-who-zeri-artist-birth-death-dates (artist-url) ;  &optional (kg :icon))
  "retrieve artist's movement from artist internal url"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             PREFIX ti: <http://www.ontologydesignpatterns.org/cp/owl/timeinterval.owl#>

SELECT DISTINCT ?birthDate ?birthStartDate ?birthEndDate ?deathDate ?deathStartDate ?deathEndDate WHERE {
                <~a> crm:P92i_was_brought_into_existence_by / crm:P4_has_time_span ?birthDate\; 
                                 crm:P93i_was_taken_out_of_existence_by / crm:P4_has_time_span ?deathDate.
                OPTIONAL {?birthDate ti:hasIntervalStartDate ?birthStartDate}
                OPTIONAL {?birthDate ti:hasIntervalEndDate ?birthEndDate}
                OPTIONAL {?deathDate ti:hasIntervalStartDate ?deathStartDate}
                OPTIONAL {?deathDate ti:hasIntervalEndDate ?deathEndDate}
                # OPTIONAL {?birthDate crm:P86_falls_within ?birthPeriod}
                # OPTIONAL {?deathhDate crm:P86_falls_within ?deathPeriod}
                } " artist-url))


;(sparql-query-zeri-artist-birth-death-dates "https://w3id.org/zericatalog/person/6225/lotto-lorenzo")
; zeri-lotto-artworks.csv

(defun sparql-who-zeri-artist-other-artworks (artist-url) ;  &optional (kg :icon))
  "retrieve artist's movement from artist internal url"
  (format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             PREFIX foaf: <http://xmlns.com/foaf/0.1/>

             SELECT DISTINCT ?art ?artLabel \(sample \(?depiction\) as ?image\) WHERE {
             ?art crm:P94i_was_created_by / crm:P14_carried_out_by  <~a>; rdfs:label ?artLabel\; foaf:depiction ?depiction.  

             FILTER \(lang\(?artLabel\) = 'it'\)
    
              } GROUP BY ?art ?artLabel  " artist-url))


;(sparql-query-zeri-artist-other-artworks "https://w3id.org/zericatalog/person/6225/lotto-lorenzo")



