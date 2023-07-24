
;;;;;;;;;
;; WHO ;;
;;;;;;;;;

; template for the query name: sparql-who-wd 

;; Wikidata 
;; -----------------------

(in-package :art-network)

; who is the author?
(defun sparql-who-wd-author (q-id &optional (kg :wikidata))
  "SPARQL query to retrieve the author"
  (format nil 
  "SELECT DISTINCT ?author. WHERE {
                   wd:~a wdt:P170 ?author. 
                   }"
  q-id))
; (sparql-who-wd-author "Q17025697")

;; Period and Place
;; -------------------------

; date of birth and death
(defun sparql-who-wd-dates-person (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the date of birth and death of the person"
  (format nil 
  "SELECT DISTINCT ?birth ?death WHERE {
                   wd:~a wdt:P569 ?birth\; wdt:P570 ?death. 
                   }"
  author-id))
; (sparql-who-wd-dates-person "Q310973")

; location of birth and death
(defun sparql-who-wd-location-birth-death-person (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the location of birth and death of the person"
  (format nil 
  "SELECT DISTINCT ?birth ?death WHERE {
                   wd:~a wdt:P19 ?birth\; wdt:P20 ?death. 
                   }"
  author-id))

; (sparql-who-wd-location-birth-death-person "Q310973")


; to which period does the person belong? 
(defun sparql-who-wd-period-person (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the historical period to which the person belong - NB: not really common on Wikidata"
  (format nil 
  "SELECT DISTINCT ?period ?periodLabel WHERE {
                   SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                   wd:~a wdt:P2348 ?period. 
                   }"
  author-id))
; (sparql-who-wd-period-person "Q310973")


; where did he work? 
; by city
(defun sparql-who-wd-author-working-city (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the cities in which the author worked"
  (format nil "SELECT DISTINCT ?place ?placeLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                wd:~a wdt:P937 ?place.
                   }"
  author-id))
; (sparql-who-wd-working-city "Q310973")
; by country

(defun sparql-who-wd-author-working-country (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the countries where the author worked"
  (format nil "SELECT DISTINCT ?place ?placeLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                wd:~a wdt:P937 / wdt:P17 ?place.
                   }"
  author-id))
; (sparql-who-wd-author-working-country "Q310973")

; where and when did he work? 

(defun sparql-who-wd-author-working-places-by-date (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the places and dates where the artist worked"
  (format nil "SELECT DISTINCT ?P937node ?place ?placeLabel ?startDate ?endDate WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language 'AUTO_LANGUAGE],en'. }
               wd:~a p:P937 ?P937node . 
               ?P937node ps:P937 ?place\; 
               pq:P580 ?startDate\;
               pq:P582 ?endDate. 

} ORDER BY ?startDate"
  author-id))
; (sparql-who-wd-author-working-places-by-date "Q5592")



;; artistic moevement or school
;; ----------------------------

;; student of P1066
(defun sparql-who-wd-author-student-of (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the teacher of the author"
  (format nil "SELECT DISTINCT ?teacher ?teacherLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                wd:~a wdt:P1066 ?teacher.
                   }"
  author-id))
; (sparql-who-wd-author-student-of "Q310973")

;; art movement: use the query sparql-what-wd-art-movement with the artist id


;; other artworks
;; ---------------------------


(defun sparql-who-wd-author-notable-work (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve other artworks made by the author marked as notable work"
  (format nil "SELECT DISTINCT ?work ?workLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                wd:~a wdt:P800 ?work.
                   }"
  author-id))
; (sparql-who-wd-author-notable-work "Q310973")

(defun sparql-who-wd-author-other-artworks (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve other artworks made by the author"
  (format nil "SELECT DISTINCT ?work ?workLabel ?image WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                ?work wdt:P170 wd:~a.
               OPTIONAL {?work wdt:P18 ?image}. 
                   }"
  author-id))
; (sparql-who-wd-author-other-artworks "Q310973")

;; religion or worldview
;; ------------------------

(defun sparql-who-wd-author-worldview (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the author's worldview"
  (format nil "SELECT DISTINCT ?view ?viewLabel ?image WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                wd:~a wdt:P140 ?view.
 
                   }"
  author-id))

(defun sparql-who-wd-author-religious-order (author-id &optional (kg :wikidata))
  "SPARQL query to retrieve the author's religious order"
  (format nil "SELECT DISTINCT ?view ?viewLabel ?image WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
                wd:~a wdt:P611 ?view.
 
                   }"
  author-id))

;; patron
;; ------------------

; Who did the creator make the visual for?


(defun sparql-who-wd-patron (id &optional (kg :wikidata))
  "SPARQL query to retrieve the patron either of the artwork or of the artist"
  (format nil "SELECT DISTINCT ?sponsor ?sponsorLabel WHERE {
               SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],en'. }
               wd:~a wdt:P88 | wdt:P859 ?sponsor.
                   }"
  id))

; (sparql-who-wd-patron "Q1231009")