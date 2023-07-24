(in-package :art-network)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Wikidata - subject ;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; all the queries that may be done to answer to what-related questions in Wikidata
;; -----------------------------------------

;; rule for name creation: sparql-<5w-question>-<kb-name>-<specific-query>. Here: sparql-what-wd 
;; type 
;; -----------------

(defun sparql-what-wd-type (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the type of the artwork"
  (format nil 
          "SELECT DISTINCT ?type ?typeLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P31 ?type. 
                   }"
          q-id))
                                        ; (sparql-who-wd-genre "Q17025697")

;; genre 
;; -----------------

(defun sparql-what-wd-genre (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the genre of the painting"
  (format nil 
  "SELECT DISTINCT ?genre ?genreLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P136 ?genre. 
                   }"
  q-id))
; (sparql-who-wd-genre "Q17025697")


;; art movement 
;; -----------------

(defun sparql-what-wd-art-movement (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the genre of the painting"
  (format nil 
  "SELECT DISTINCT ?movement ?movementLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P135 ?movement. 
                   }"
  q-id))
; (sparql-who-wd-art-movement "Q17025697")

;; material
;; ----------------

(defun sparql-what-wd-material (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the material of the painting from Wikidata"
  (format nil 
  "SELECT DISTINCT ?material ?materialLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P186 ?material. 
                   }"
  q-id))
; (sparql-what-wd-material "Q17025697")


;; main subject 
;; -----------------------------------------

(defun sparql-what-wd-main-subject (q-id &optional (kg :wikidata))
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  ;; SPARQL query to retrieve the main subject of the painting.
  (format nil 
  "SELECT DISTINCT ?subject ?subjectLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P921 ?subject. 
                   }"
  q-id))
; (wd-sparql-query-main-subject "Q17025697")


(defun sparql-what-wd-painting-main-subject (subj-id q-id &optional (kg :wikidata))
  "SPARQL query to retrieve all the paintings having the same main subject."
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  (format nil 
  "SELECT DISTINCT ?art ?artLabel WHERE {
              SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
              ?art wdt:P921 wd:~a. 
              FILTER \(?art != wd:~a\)
              }"
  q-id))
; (wd-sparql-query-painting-main-subject "Q112143886")



(defun sparql-what-wd-painting-main-subject-same-movement (q-id subj-id movements-id-string &optional (kg :wikidata))
  "SPARQL query to retrieve all the paintings having the same main subject."
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  (format nil 
  "SELECT DISTINCT ?art ?artLabel ?image WHERE {
              SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
              VALUES ?movement {~a}
              ?art wdt:P921 wd:~a \; wdt:P18 ?image. 
              {?art wdt:P170 / wdt:P135 ?movement} UNION {?art wdt:P135 ?movement} 
              FILTER (?art != wd:~a)
              
              }"
  movements-id-string subj-id q-id))
; (sparql-what-wd-painting-main-subject-same-movement "Q4009580" "Q117007545" "wd:Q1474884 wd:Q131808")


(defun sparql-what-wd-painting-main-iconclass (iconclass-code &optional (kg :wikidata))
  "SPARQL query to retrieve all the paintings having the same iconclass notation."
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  (format nil 
  "SELECT DISTINCT ?art ?artLabel WHERE {
 SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
        ?art wdt:P921 / wdt:P1256 '~a'. 
  }"
  iconclass-code))
; (wd-sparql-query-painting-main-iconclass "92C4")

(defun wd-sparql-query-what-painting-main-subject-movement (q-id &optional (kg :wikidata))
  " SPARQL query to retrieve all the paintings of the same art movement having the same main subject."
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.
                          (setf id2 q-id)
  (format nil   "SELECT DISTINCT ?art ?artLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'.}
               wd:~a wdt:P921 ?subject\;
                       wdt:P135 ?movement. 
               ?art2 wdt:P921 ?subject\;
                      wdt:P135 ?movement. 
               FILTER \(?art2 != wd:~a\) }" q-id id2))
; (wd-sparql-query-painting-main-subject-movement "Q490312")


(defun wd-sparql-query-what-painting-main-subject-genre (q-id &optional (kg :wikidata))
  " SPARQL query to retrieve all the paintings of the same art genre (e.g. religious art) having the same main subject."
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.
                          (setf id2 q-id)
  (format nil   "SELECT DISTINCT ?art ?artLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'.}
               wd:~a wdt:P921 ?subject\;
                       wdt:P136 ?genre. 
               ?art2 wdt:P921 ?subject\;
                      wdt:P136 ?genre. 
               FILTER \(?art2 != wd:~a\) }" q-id id2))
; (wd-sparql-query-painting-main-subject-genre "Q490312")


;; depicts
;; -----------------------------------------


(defun wd-sparql-what-query-depicts (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the objects depicted in a painting."
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.

  ;; SPARQL query to retrieve the main subject of the painting.
  (format nil 
  "SELECT DISTINCT ?subject ?subjectLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P180 ?subject. 
                   }"
  q-id))
; (wd-sparql-query-depicts "Q17025697")

; symbol of depict
; image region
; color


;; sources
;; -----------------------------------------

(defun wd-sparql-query-what-source (q-id &optional (kg :wikidata))
  " SPARQL query to retrieve, if present, the sources on which the painting theme is based"
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.
                
  (format nil   "SELECT DISTINCT ?source ?sourceLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'.}
               wd:~a wdt:P144 ?source.
                        }" q-id))
; (wd-sparql-query-source "Q490312")


(defun wd-sparql-query-what-subject-source (q-id &optional (kg :wikidata))
  " SPARQL query to retrieve, if present, the sources on which the painting theme is based"
  (declare (ignorable kg)) ;; We may include other knowledge grapgs (kg) at some point
                           ;; or another RDF triplestore, but for now we assume only wikidata.
                
  (format nil   "SELECT DISTINCT ?theme ?themeLabel ?source ?sourceLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'.}
                 wd:~a wdt:P921 | wdt:P180 ?theme. 
                 ?theme wdt:P144 ?source. }
                        }" q-id))
; (wd-sparql-query-what-subject-source "Q107135618")


;; art movement and subjects
;; SPARQL query to retrieve all the artworks of the same movement with the same subjects. Input: string of subjects with wd: prefix, country, (wd id); start date, end date (integers)

(defun wd-sparql-query-what-artworks-period-subjects (subjects-id-string country-id start-year end-year &optional (kg :wikidata))
(format nil "SELECT DISTINCT ?artwork ?startDate ?artworkLabel ?image WHERE {
                                            SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                                            ?artwork wdt:P180 ~a. # subjects
                                            ?artwork wdt:P170 / wdt:P937 / wdt:P17 wd:~a. # made by an artist who worked in country
                                            OPTIONAL { ?artwork wdt:P571 ?startDate. }
                                            OPTIONAL { ?artwork wdt:P18 ?image. }
                                            FILTER (year(?startDate) >= ~a && year(?startDate) <= ~a) # In the High Renaissance period
                                            }

                                            ORDER BY ?startDate " subjects-id-string country-id start-year end-year))


; example with all the artworks depicting Venus and Cupid from the high renaissance in Italy 
;(sparql-query-wd-artworks-period-subjects "wd:Q47652, wd:Q5011" "Q38" "1494" "1527")

