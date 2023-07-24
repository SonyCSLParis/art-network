;;;;;;;;;;;;;;;;;;;;
;; WHEN questions ;;
;;;;;;;;;;;;;;;;;;;;

(in-package :art-network)

;; Wikidata
;;--------------------------------

; artwork year of creation P571 (inception - year of begin of existence)"
(defun sparql-when-wd-creation-date (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the date of beginning of existence of the artwork"
  (format nil 
  "SELECT DISTINCT ?date WHERE {
                   wd:~a wdt:P571 ?date. 
                   }"
  q-id))
; (wd-sparql-query-creation-date "Q17025697")


; artwork period P2348 (time period)
(defun sparql-when-wd-period (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the historical period to which the artwork belong"
  (format nil 
  "SELECT DISTINCT ?period WHERE {
                   wd:~a wdt:P2348 ?period.  
                   }"
  q-id))
; (wd-sparql-query-period "Q18748378") ; an artwork having as Renaissance as time period


; other historical events occurring in the same period wd:Q110227435 (past occurrence)
(defun sparql-when-wd-historical-events (start-date end-date country-id &optional (kg :wikidata))
  "SPARQL query to retrieve the events that started between the artist birthday and the creation of the painting in the same country where the artist worked"
  (format nil 
       "SELECT DISTINCT ?event ?eventLabel ?locationLabel ?start ?end WHERE {
        SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
       ?event wdt:P31/wdt:P279* wd:Q110227435. 
       ?event wdt:P580 ?start\; wdt:P582 ?end\; wdt:P276 ?location. 
       ?location wdt:P17 wd:~a. 
       FILTER \(year\(?start\) >= ~a && year \(?start\) <= ~a\)
  
      } ORDER BY ?start"
  country-id start-date end-date))

; (wd-sparql-query-historical-events "1480" "1530" "Q38")

