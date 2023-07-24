(in-package :art-network)


; wikidata
;---------------------------------


; retrieve the italian title of the artwork. Useful for italian knowledge bases such arco and Zeri & lode

(defun wd-ita-label-from-artist-title (artist title &optional (kg :wikidata))
  "sparql to retrieve the italian label of the painting from the artist and title english labels"
  (format nil 
  "SELECT DISTINCT ?artwork ?artworkLabel where {
                   ?artwork wdt:P170 ?artist\; rdfs:label '~a'@en, ?artworkLabel.
                   ?artist rdfs:label '~a'@en. 
                   FILTER \(lang\(?artworkLabel\) = 'it'\)
   }"
  title artist))
; (wd-ita-label-from-artist-title "Lorenzo Lotto" "Venus and Cupid")

(defun wd-ita-label-from-id (wd-id &optional (kg :wikidata))
  "sparql to retrieve the italian label of the painting from the wikidata id"
  (format nil 
  "SELECT DISTINCT ?artworkLabel where {
                   wd:~a rdfs:label ?artworkLabel.
                   FILTER \(lang\(?artworkLabel\) = 'it'\)
   }"
  wd-id))
; (wd-ita-label-from-id "Q4009580")



;; Zeri & Lode artwork ID
;; ----------------------------------------------

; query to search an arwork filtering by title and string labels
; beofre looking for the art ID, we should find the italian title from wikidata. 
; this can be done with the queries wd-ita-label-from-artist-title or wd-ita-label-from-id in id-and-metadata.lisp

; aim: constructing a query which has a single filter for every single word in the artist's name string
; help function to split the name of the artist in a string of strings
(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;(setf lotto-name (split-by-one-space "Lorenzo Lotto"))


; this is not working
; query to format a sparql filter for each name in the list given by the previous query 

;(defun filter-by-words (list) 
;  "returns a sparql filter expression for every word in a list"
;  (loop for word in list do
;        (format nil "FILTER\(regex\(str\(?artistLabel\), \('~a'\), 'i'\)\)" word)))

; to do: query that concatenate th actual query with the filter-by-words strings 


(defun sparql-query-zeri-artwork-id (artist-name artist-surname it-title) ;  &optional (kg :icon))
(format nil "PREFIX fabio: <http://purl.org/spar/fabio/>
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>
             PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             SELECT DISTINCT ?art ?label ?artist WHERE {
             ?art a fabio:ArtisticWork ;
                rdfs:label ?label;
                crm:P94i_was_created_by / crm:P14_carried_out_by ?artist. 
             ?artist rdfs:label ?artistLabel.
             FILTER(regex(str(?artistLabel), ('~a'), 'i'))
             FILTER(regex(str(?artistLabel), ('~a'), 'i'))
             FILTER(regex(str(?label), ('~a'), 'i'))
    
              } " artist-name artist-surname it-title))


;(sparql-query-zeri-artwork-id "Lorenzo" "Lotto" "Venus")



;; Iconology Dataset
;; -----------------------------------------------------

(defun sparql-query-icon-artwork-id (artist-name artist-surname it-title) ;  &optional (kg :icon))
(format nil "PREFIX icon: <https://w3id.org/icon/ontology/> 
             PREFIX crm: <http://www.cidoc-crm.org/cidoc-crm/>

             SELECT DISTINCT ?artwork WHERE {
        
             ?obj crm:P65_shows_visual_item ?artwork\; ^crm:P108_has_produced / crm:P14_carried_out_by ?artist.
             ?artist rdfs:label ?artistLabel. 
             ?obj crm:P102_has_title / rdfs:label ?titleLabel. 
             FILTER\(regex\(str\(?artistLabel\), \('~a'\), 'i'\)\))
             FILTER\(regex\(str\(?titleLabel\), \('~a'\), 'i'\)\)
             } LIMIT 10" artist-name artist-surname it-title))


;(sparql-query-icon-artwork-id "Lorenzo" "Lotto" "Venus")


;; HyperReal
;; ----------------------

(defun sparql-query-symbol-id (symbol-word   &optional (kg :symb-dataset))
 "query to search by keyword the ID of symbols that may have a symbolical meaning"

  (format nil "prefix kb: <https://w3id.org/simulation/data/> 
               prefix sim: <https://w3id.org/simulation/ontology/> 

               SELECT DISTINCT ?sim WHERE {
                   ?sim sim:isSimulacrumOf ?symbol\; rdfs:label ?simLabel. 

                   FILTER\(regex\(str\(?simLabel\), \('~a'\), 'i'\)\)  
                   OPTIONAL {?symbol sim:hasContext ?context}
                
    }" symbol-word))
; (sparql-query-symbol-id "Venus")
