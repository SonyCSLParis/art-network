; inn usage example file: systems>inn
; documentation: systems>inn



;; This file recreates the fist network with the INN package and the core 5W quesitons as a starting point. 
;;--------------------------------------------------------------------------------------------------------

;; to see the graph, go to localhost:8000/ in your browser
; (ql:quickload :art-network)
; (in-package :art-network)
; (ql:quickload "cl-csv")

; (ql:quickload :inn)

(in-package :inn)
(defparameter *lotto-inn* (make-instance 'integrative-narrative-network))
(draw-inn-network-in-vis-js *lotto-inn*)

(progn

; definition of the colors according to the source or type of insert

(defmethod get-node-color ((type (eql :manual)))
  (declare (ignore type))
  "#FB7E81")


(defmethod get-node-color ((type (eql :automatic)))
  (declare (ignore type))
  "#525ace")

(defmethod get-node-color ((type (eql :textrazor)))
  (declare (ignore type))
  "#C2FABC")


(defmethod get-node-color ((type (eql :kb-wikidata)))
  (declare (ignore type))
  "#FFF7AD")

(defmethod get-node-color ((type (eql :kb-symbol)))
  (declare (ignore type))
  "#D2A242")

(defmethod get-node-color ((type (eql :kb-icon)))
  (declare (ignore type))
  "#ffc947")



(defmethod get-node-color ((type (eql :kb-manual))) ;manually found in kb
  (declare (ignore type))
  "#ffa794")

(defmethod get-node-color ((type (eql :kb-text))) ; found in kb and then cited by the text
  (declare (ignore type))
  "#feb2f8")



(defmethod get-node-color ((type (eql :fcg))) ;fcg semantic frames
  (declare (ignore type))
  "#8fbdff")

(defmethod get-node-color ((type (eql :wn))) ;wordnet
  (declare (ignore type))
  "#7cfbe3")

(defmethod get-node-color ((type (eql :kb-zeri))) ;wordnet
  (declare (ignore type))
  "#FFA164")

(defmethod get-node-color ((type (eql :kb-arco))) ;wordnet
  (declare (ignore type))
  "#92694F")

; I still have to decide if it's better to add a circular shape. 
; in this case, follow the example for evert type: 

;(defmethod get-node-shape ((type (eql :wn)))
;  (declare (ignore type))
;  "circle")

)

; color node tests

;(inn-add-node *lotto-inn* (make-inn-node :label "From wordnet" :type :wn))
;(inn-add-node *lotto-inn* (make-inn-node :label "Manual insertion" :type :manual))
;(inn-add-node *lotto-inn* (make-inn-node :label "textrazor" :type :textrazor))
;(inn-add-node *lotto-inn* (make-inn-node :label "fcg" :type :fcg))
;(inn-add-node *lotto-inn* (make-inn-node :label "kb-wikidata" :type :kb-wikidata))
;(inn-add-node *lotto-inn* (make-inn-node :label "kb-symbol" :type :kb-symbol))
;(inn-add-node *lotto-inn* (make-inn-node :label "kb-icon" :type :kb-icon))
;(inn-add-node *lotto-inn* (make-inn-node :label "automatic" :type :automatic))

; now we add the starting nodes, the question nodes and the metadata

(progn

(setq *initial-nodes*
  (list (make-inn-image   :type :inn-image
                          :description "Lorenzo Lotto, Venus and Cupid"
                          :label "Lorenzo Lotto, Venus and Cupid"
                          :url "https://upload.wikimedia.org/wikipedia/commons/3/39/1525_Lotto_Venus_und_Amor_anagoria.JPG") ; node 0
        (make-narrative-question :label "Who?") ; Node 1
        (make-narrative-question :label "What?") ; Node 2
        (make-narrative-question :label "When?") ; Node 3
        (make-narrative-question :label "Where?") ; Node 4
        (make-narrative-question :label "Why?"))) ; Node 5



;; We add our starting node:
(inn-add-node *lotto-inn* (nth 0 *initial-nodes*))

;; This painting raises the 5w questions and sub-questions:
;;---------------------------------------------------------
(inn-add-edges-to-node "Lorenzo Lotto, Venus and Cupid" *initial-nodes*)


(setq *secondary-what-question-nodes*
  (list (make-narrative-question :label "Material?")
        (make-narrative-question :label "Genre?") 
        (make-narrative-question :label "Art Movement?") 
        (make-narrative-question :label "Subject?")
        (make-narrative-question :label "Title?")
        (make-narrative-question :label "Type?")))

(setq *secondary-who-question-nodes*
  (list (make-narrative-question :label "Author?")
        (make-narrative-question :label "Patron?")))

(setq *secondary-when-question-nodes*
  (list (make-narrative-question :label "Art Practice?")
        (make-narrative-question :label "Cultural Tradition?")))

;(inn-add-edges-to-node "Author?" (list (make-inn-node :label "Lorenzo Lotto" :type :textrazor)))

(inn-add-edges-to-node "What?" *secondary-what-question-nodes*)
(inn-add-edges-to-node "When?" *secondary-when-question-nodes*)

(inn-add-edges-to-node "Who?" *secondary-who-question-nodes*) 



(setq *lotto-painting-metadata*  (list (make-inn-node :label "Title - Venus and Cupid" :type :manual)
                                       (make-inn-node :label "Lorenzo Lotto" :type :textrazor)
                                       (make-inn-node :label "oil painting" :type :textrazor)
                                       (make-inn-node :label "painting" :type :kb-wikidata)
                                       (make-inn-node :label "Venice" :type :textrazor)
                                       (make-inn-node :label "Loreto" :type :textrazor)
                                       (make-inn-node :label "Charles Bierer Wrightsman" :type :textrazor)))


(inn-add-nodes *lotto-inn* *lotto-painting-metadata*)


(inn-answer-question-by-adding-edge "Author?" "Lorenzo Lotto")
(inn-answer-question-by-adding-edge "Material?" "oil painting")
(inn-answer-question-by-adding-edge "Title?" "Title - Venus and Cupid")
(inn-answer-question-by-adding-edge "Type?" "painting")
(inn-add-label-edge-from-nodes-label "Lorenzo Lotto, Venus and Cupid" "Charles Bierer Wrightsman" "gifted by")


;;;;;;;;;;;;;;
;; Wikidata ;;
;;;;;;;;;;;;;;

; queries to Wikidata on specific aspects
;------------------------------------------------


;; WHAT 
;; -------------

;; genre P136 
; query: sparql-what-wd-genre

(inn-answer-question-by-adding-edge-and-nodes "Genre?" '("nude" "mythological painting") :kb-wikidata)

; art movement - P135
; query: sparql-what-wd-art-movement
; for this painting, we do not have an art movement related to it. We should retrieve it from the author
; we do the same query with the author's id in spite of the artwork's id: Q310973

(inn-answer-question-by-adding-edge-and-nodes "Art Movement?" '("High Renaissance" "mannerism") :kb-wikidata) 

; since we discovered this through the author's details, we add an edge between Lorenzo Lotto and Art Movement
(inn-add-edges-by-nodes-label "Lorenzo Lotto" "Art Movement?")

;; depicts - P180
;; sparql query: wd-sparql-what-query-depicts
(setq *wikidata-depicts* '("nudity" "Hedera" "Venus" "Venus" "snake" "urolagnia" "conch"
                                          "aspic viper" "woman" "smile" "nude" "rose" "grass" "fascia"
                                          "standing" "veil" "ribbon" "swagger stick" "diadem" "switch"
                                          "trunk" "areola" "petal" "seashell" "censer" "half reclining"
                                          "bird's wing" "earring" "bracelet" "laurel wreath" "urination"
                                          "pubic hair removal" "drapery" "navel" "nude"))


(inn-add-narrative-question-and-answer "Subject?" "Depicts?" *wikidata-depicts* :kb-wikidata)


;; main subject
; query: sparql-what-wd-main-subject
(inn-add-label-edge-and-node "Subject?" "Venus and Cupid" "main subject" :kb-wikidata)
(inn-add-label-edge-and-node "Venus and Cupid" "Artistic theme" "instance of" :kb-wikidata)
(inn-add-label-edge-and-node "Venus and Cupid" "92C454" "iconclass notation" :kb-wikidata)


;; sources - P144
;; query: wd-sparql-query-what-source
(inn-add-narrative-question-and-answer "What?" "Source?" '("epithalamium") :kb-wikidata)

;; check if the depicted subjects have a source: 
;; query: wd-sparql-query-what-subject-source
;; no further sources for this painting


;; material - P186
;; query: sparql-what-wd-material
(inn-answer-question-by-adding-edge-and-nodes "Material?" '("oil paint" "canvas") :kb-wikidata) 


;; other artworks of the same art movements with the same main subject
;; query: sparql-what-wd-painting-main-subject-same-movement 

; (inn-open-csv-and-add-inn-image-to-node "../LottoProject/query-art-same-subj-movement.csv" "Subject?" "Artworks of the same movement with the same subject?")

(defparameter *same-subj-artworks-table-image* (cl-csv:read-csv #P"grammars/art-network/paintings/data/query-art-same-subj-movement.csv")
"Contains the original table with the artworks of the same period with the same main subject having an image in list form.")

(setf *same-subj-artwork-list* (loop for line in *same-subj-artworks-table-image* collect (make-inn-image   :type :inn-image
                          :label (second line)
                          :description (second line)
                          :url (third line))))
(inn-add-edges-to-node "What?" (list (make-answered-narrative-question :label "Artworks of the same movement with the same subject?")))
(inn-add-edges-to-node "Artworks of the same movement with the same subject?" *same-subj-artwork-list*)

;; WHO
;; ----------

;; Who is the author?
;; query: sparql-who-wd-author 
;; already answered

;; when did he live? 
;; query: sparql-who-wd-dates-person 

(setq *time-period*
  (list (make-answered-narrative-question :label "Time period?")
        (make-inn-node :label "1480" :type :kb-wikidata) 
        (make-inn-node :label "1556" :type :kb-wikidata)))


(inn-add-nodes *lotto-inn* *time-period*)
(inn-add-edges-by-nodes-label "Lorenzo Lotto" "Time period?")
(inn-add-edges-by-nodes-label "When?" "Time period?") ; since the query solves also the when question, we bound the variable also to it
(inn-add-label-edge-from-nodes-label "Time period?" "1480" "birth date")
(inn-add-label-edge-from-nodes-label "Time period?" "1556" "death date")

; what are the places of bith and death? 
; query: sparql-who-wd-location-birth-death-person
; answer: Venice and Loreto. Since we already have them, we just add a labeled relation
(inn-add-node *lotto-inn* (make-narrative-question :label "Location?"))
(inn-add-edges-by-nodes-label "Lorenzo Lotto" "Location?")
(inn-add-label-edge-from-nodes-label "Location?" "Venice" "birth place")
(inn-add-label-edge-from-nodes-label "Location?" "Loreto" "death place")
(inn-answer-question (inn-find-node-by-label (get-current-inn) "Location?"))

; when and where did he work? 
; query: sparql-who-wd-author-working-places-by-date
; no answer for Lorenzo Lotto. 

; where did he work? 
;; query: sparql-who-wd-author-working-city

(setq *lotto-work-locations* '("Rome" "Ancona" "Treviso" "Bergamo")) ; since Venice is already in the graph, we remove it from the list - otherwise, a new node will be created
(inn-add-narrative-question-and-answer "Location?" "Work Location?" *lotto-work-locations* :kb-wikidata)
(inn-add-edges-by-nodes-label "Work Location?" "Venice") ; now we add Venice

;; to which period does he belong? 
;; query: sparql-who-wd-period-person 
;; no period given for Lorenzo Lotto. 


;; what other artworks did he do? 
;; query: sparql-who-wd-author-other-artworks 
;; to do: make the query and format the label and image results to create an inn-image node
(inn-add-edges-to-node "Lorenzo Lotto" (list (make-narrative-question :label "Other artworks?")))

;;
(defparameter *lotto-artworks-table-image* (cl-csv:read-csv #P"grammars/art-network/paintings/data/query-other-artworks-with-image.csv")
"Contains the original table with the lotto artworks having an image in list form.")

(setf *lotto-artwork-list* (loop for line in *lotto-artworks-table-image* collect (make-inn-image   :type :inn-image
                          :label (second line)
                          :description (second line)
                          :url (third line))))
(inn-add-edges-to-node "Lorenzo Lotto" (list (make-answered-narrative-question :label "Other artworks?")))
(inn-add-edges-to-node "Other artworks?" *lotto-artwork-list*)

; the following code raises an error 
;(let* (lotto-artwork-list (loop for line in *lotto-artworks* collect (make-inn-image   :type :inn-image
;                          :label (second line)
;                          :url (third line))))
;               (inn-add-edges-to-node "Other artworks?" lotto-artwork-list))


;; religion or worldview
; query: sparql-who-wd-author-worldview
; no worldview for Lotto

; religious oder
; query: sparql-who-wd-author-religious-order

(inn-add-node *lotto-inn* (make-inn-node :label "Franciscans" :type :kb-wikidata))
(inn-add-label-edge-from-nodes-label "Lorenzo Lotto" "Franciscans" "religious order")

; patron
; query: sparql-who-wd-patron 
; no result for Lotto or for the paiting patron



;; WHEN 
;;-----------
; date can be retrieved with the query template sparql-when-wd-creation-date
; date: 1530. 
(inn-add-label-edge-and-node "When?" "1530" "inception" :kb-wikidata)

; time period can be retrieved with the query template sparql-when-wd-period. No time period for this artwork

; time period can be retrieved also through the birth and death dates of the artist, as we previously did.  


(inn-add-edges-by-nodes-label "When?" "Time period?") ; since the query solves also the when question, we bound the variable also to it


;; historical and cultural context
;; what events can the author have seen? 
;; select historical events occurring in the countries where the artist lived from his birth date to the date of creation of the artwork. 
; the country can be retrieved through the query wd-sparql-query-author-working-country
; query to retrieve the events : sparql-when-wd-historical-events with input: "1480" "1530" "Q38"
; we decide to not insert them, since none of them is happening in Venice. 

)

;;;;;;;;;;;;;;;;;;;;;;;
;; Iconology dataset ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; find the ID of the artwork through title and artist name regex
;; query: sparql-query-icon-artwork-id
;; ID: ART1002test

; WHAT 
; --------------

;; level 1 subjects
;; query: sparql-what-icon-level1
(progn
(setf *level1_subjects* '("belt" "bow" "earring" "knotted ribbon bracelet" "peeing through a wreath" "putto"
                          "rose" "rose petal" "tiara" "touching breast" "wings" "woman" "holding a wreath"
                          "myrtle wreath" "veil" "playful attitude" "cloth" "cone shell" "incense burner" "ivy"
                          "rod" "serpent"))


(loop for name in *level1_subjects* do (inn-add-node *lotto-inn* (make-inn-node :label name :type :kb-icon)))
(loop for name in *level1_subjects* do (inn-add-label-edge-from-nodes-label "Depicts?" name  "first level subject"))

;(inn-add-edges-to-node "Subject?" (list (make-answered-narrative-question :label "Subjects per level?")))
;(inn-add-narrative-question-and-answer "Subjects per level?" "Level 1 subjects?" *level1_subjects* :kb-icon)


; to do: find a way to detect duplicate nodes
;(detect-duplicates *labels-list*)

;; level 1 subjects qualities
;; query: sparql-what-icon-level1-quality
(setf *level-1-qualities* '(("earring" "cardinality two")
("wings" "cardinality two")
("putto" "nudity")
("woman" "nudity")
("cloth" "red color")
("knotted ribbon bracelet" "multiple quantity")
("rose petal" "multiple quantity")
("woman" "pose half stretched out")))

(loop for line in *level-1-qualities* do (inn-add-node *lotto-inn* (make-inn-node :label (second line) :type :kb-icon)))
(loop for line in *level-1-qualities* do (inn-add-label-edge-from-nodes-label (first line) (second line) "has quality"))


; no compositional structure for this painting. 
; query: sparql-what-icon-level1-structure

;; level 2 subjects - 
(setf *level2_subjects* '("Venus' belt" "incense burner-bridal chamber decoration" "knotted ribbon bracelets-love" "myrtle wreath-conjugal love"
                          "red cloth-bridal chamber decoration" "Venus' belt-seduction" "cone shell-fertility" "ivy-eternal love"
                          "rod-venus punished Cupid" "serpent-pitfalls of love" "Cupid" "puer mingens" "rose-virginity" "playful attitude-joy, love" "Venus"
                          "venetian bridal clothes" "urinating-wish of fertility" "puer mingens-fertility" "flower of Venus" "decoration for wedding occasions"))

;(inn-add-narrative-question-and-answer "Subjects per level?" "Level 2 subjects?" *level2_subjects* :kb-icon)
(loop for name in *level2_subjects* do (inn-add-node *lotto-inn* (make-inn-node :label name :type :kb-icon)))
(loop for name in *level2_subjects* do (inn-add-label-edge-from-nodes-label "Depicts?" name  "second level subject"))
;; corrpesp. to level 1



(setf *level2-to-level1* '(("Venus' belt" "belt")
                           ("incense burner-bridal chamber decoration" "incense burner")
                           ("knotted ribbon bracelets-love" "knotted ribbon bracelet")
                           ("myrtle wreath-conjugal love" "myrtle wreath")
                           ("Venus" "belt")
                           ("playful attitude-joy, love" "bow")
                           ("Venus" "earring")
                           ("venetian bridal clothes" "earring")
                           ("Venus" "knotted ribbon bracelet")
                           ("urinating-wish of fertility" "peeing through a wreath")
                           ("puer mingens-fertility" "peeing through a wreath")
                           ("urinating-wish of fertility" "putto")
                           ("playful attitude-joy, love" "putto")
                           ("puer mingens-fertility" "putto")
                           ("Venus" "rose")
                           ("flower of Venus" "rose")
                           ("decoration for wedding occasions" "rose")
                           ("Venus" "rose petal")
                           ("flower of Venus" "rose petal")
                           ("decoration for wedding occasions" "rose petal")
                           ("Venus" "tiara")
                           ("venetian bridal clothes" "tiara")
                           ("Venus" "touching breast")
                           ("playful attitude-joy, love" "wings")
                           ("Venus" "woman")
                           ("urinating-wish of fertility" "woman")
                           ("Venus" "holding a wreath")
                           ("urinating-wish of fertility" "holding a wreath")
                           ("playful attitude-joy, love" "holding a wreath")
                           ("puer mingens-fertility" "holding a wreath")
                           ("urinating-wish of fertility" "myrtle wreath")
                           ("Venus" "veil")
                           ("venetian bridal clothes" "veil")
                           ("Cupid" "bow")
                           ("puer mingens" "peeing through a wreath")
                           ("Cupid" "putto")
                           ("puer mingens" "putto")
                           ("rose-virginity" "rose")
                           ("rose-virginity" "rose petal")
                           ("Cupid" "wings")
                           ("Cupid" "holding a wreath")
                           ("puer mingens" "holding a wreath")
                           ("playful attitude-joy, love" "playful attitude")
                           ("Cupid" "playful attitude")
                           ("red cloth-bridal chamber decoration" "cloth")
                           ("Venus' belt-seduction" "belt")
                           ("cone shell-fertility" "cone shell")
                           ("ivy-eternal love" "ivy")
                           ("rod-venus punished Cupid" "rod")
                           ("serpent-pitfalls of love" "serpent")))

(loop for line in *level2-to-level1* do (inn-add-label-edge-from-nodes-label (first line) (second line) "composed of"))


;; level 3 subjects

;(inn-add-narrative-question-and-answer "Subjects per level?" "Level 3 subjects?" '("wedding wish") :kb-icon)
(inn-add-node *lotto-inn* (make-inn-node :label "wedding wish" :type :kb-icon))
(inn-add-label-edge-from-nodes-label "Depicts?" "wedding wish" "third level subject")

;; level 3 subjects specifically referring to a particular subject
;; query: sparql-what-icon-level3-refer-to

(setf *from-level3-to-subjects* '(("wedding wish" "cone shell-fertility")
                                  ("wedding wish" "decoration for wedding occasions")
                                  ("wedding wish" "incense burner-bridal chamber decoration")
                                  ("wedding wish" "ivy-eternal love")
                                  ("wedding wish" "myrtle wreath-conjugal love")
                                  ("wedding wish" "playful attitude-joy, love")
                                  ("wedding wish" "puer mingens-fertility")
                                  ("wedding wish" "red cloth-bridal chamber decoration")
                                  ("wedding wish" "venetian bridal clothes")
                                  ("wedding wish" "Venus' belt")
                                  ("wedding wish" "Venus' belt-seduction")))


(loop for line in *from-level3-to-subjects* do (inn-add-label-edge-from-nodes-label (first line) (second line) "has"))


;; subjects' sources
;; query: sparql-what-icon-recognition-sources

(inn-add-node *lotto-inn* (make-inn-node :label "Cesare Vecellio, De gli habiti antichi, e moderni di diverse parti del mondo libri due" :type :kb-icon))
(inn-add-label-edge-from-nodes-label "venetian bridal clothes" "Cesare Vecellio, De gli habiti antichi, e moderni di diverse parti del mondo libri due" "cites as evidence")

;; symbols' sources 
;; query: sparql-what-icon-symbols-sources

(setf *symbols-evidence* '(("incense burner-bridal chamber decoration" "Sidonius, Epithalamium")
                           ("incense burner-bridal chamber decoration" "Claudianus, The Magnate")
                           ("Venus' belt-seduction" "Homerus, Iliad")
                           ("red cloth-bridal chamber decoration" "Sidonius, Epithalamium")
                           ("red cloth-bridal chamber decoration" "Claudianus, The Magnate")
                           ("urinating-wish of fertility" "Gaius Valerius Catullus, Liber")
                           ("ivy-eternal love" "Gaius Valerius Catullus, Liber")
                           ("puer mingens-fertility" "Gaius Valerius Catullus, Liber")))

(loop for line in *symbols-evidence* do (inn-add-node *lotto-inn* (make-inn-node :label (second line) :type :kb-icon)))
(loop for line in *symbols-evidence* do (inn-add-label-edge-from-nodes-label (first line) (second line) "cites as evidence"))



;;;;;;;;;;;;;;;
;; Zeri&Lode ;;
;;;;;;;;;;;;;;;

;; you can perform queries on the sparql web interface: http://data.fondazionezeri.unibo.it/query/

;; find the id from artwork and artist's labels
;; query: sparql-query-zeri-artwork-id
;; id: "https://w3id.org/zericatalog/artwork/47929"




;; WHO
;; -------------------


;; artist's name: 
; (sparql-who-zeri-artist "47929")
; answer: id: https://w3id.org/zericatalog/person/6225/lotto-lorenzo , label: "Lotto Lorenzo" 



;; artists' movement: (sparql-who-zeri-artist-movement "https://w3id.org/zericatalog/person/6225/lotto-lorenzo")

(inn-add-edges-to-node "Art Movement?" (list (make-inn-node :label "scuola italiana" :type :kb-zeri)
                                         (make-inn-node :label "scuola veneta" :type :kb-zeri)))

;; birth and death date: 
;(sparql-who-zeri-artist-birth-death-dates "https://w3id.org/zericatalog/person/6225/lotto-lorenzo")
; we add only the death date, since it slightly vary from the one given by Wikidata
(inn-add-node *lotto-inn*  (make-inn-node :label "1556-1557" :type :kb-zeri))
(inn-add-label-edge-from-nodes-label "1556-1557" "Time period?" "death date")


;; other artworks by the same artist
;; (sparql-who-zeri-artist-other-artworks "https://w3id.org/zericatalog/person/6225/lotto-lorenzo")
; results stored in "../LottoProject/zeri-lotto-artworks.json"


;; WHAT
;;----------

; subject
; (sparql-query-zeri-artwork-subject "https://w3id.org/zericatalog/artwork/47929")
; Venere e Cupido
; more detail about the subject: ?subj  rdfs:seeAlso ?else. result:  iconclass: 92C454
; same iconclass notation of the Wikidata's main theme. 

(inn-add-node *lotto-inn*  (make-inn-node :label "Venere e Cupido" :type :kb-zeri))
(inn-add-edges-by-nodes-label "Subject?" "Venere e Cupido")
(inn-add-edges-by-nodes-label "92C454" "Venere e Cupido")

;;;;;;;;;;
;; Arco ;;
;;;;;;;;;;




)

;; STEP 2
;; -------------------------------------------


; retrieve more information about some entities of interest 

;;;;;;;;;;;;;;
;; Wikidata ;;
;;;;;;;;;;;;;;

;; retrieve more details about the entities of interest from Wikidata. 


;; we focus on the characters in the title (Venus and Cupid)

;; add Venus details
(progn
(defvar venus_details '(("P21" "sex or gender" "Q6581072" "female")
                        ("P40" "child" "Q5011" "Cupid")
                        ("P31" "instance of" "Q205985" "goddess")
                        ("P31" "instance of" "Q1470705" "fertility deity")
                        ("P31" "instance of" "Q11688446" "Roman deity")
                        ("P460" "said to be the same as" "Q35500" "Aphrodite")
                        ("P1256" "Iconclass notation" "92C4" "92C4")))

(loop for line in venus_details do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-wikidata)))
(loop for line in venus_details do (inn-add-label-edge-from-nodes-label "Venus" (fourth line) (second line)))

;; retrieve more details for Afrodite

;(defparameter *afrodite-details* nil)
;(setf *afrodite-details* (my-filter-query-results "Q35500"))

(setf afrodite_details  '(("P40" "child" "Q121973" "Eros") 
                            ("P361" "part of" "Q101609" "Twelve Olympians") 
                            ("P31" "instance of" "Q22989102" "Greek deity") 
                            ("P21" "sex or gender" "Q6581072" "female") 
                            ("P31" "instance of" "Q113103481" "Olympian god") 
                            ("P31" "instance of" "Q205985" "goddess") 
                            ("P31" "instance of" "Q1470705" "fertility deity") 
                            ("P2925" "domain of saint or deity" "Q7242" "beauty") 
                            ("P5986" "Getty Iconography Authority ID" "901000612" "901000612") 
                            ("P2925" "domain of saint or deity" "Q316" "love")))

(loop for line in afrodite_details do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-wikidata)))
(loop for line in afrodite_details do (inn-add-label-edge-from-nodes-label "Aphrodite" (fourth line) (second line)))

;; add cupid details
;(setf *cupid-details* (my-filter-query-results "Q5011"))
(defvar cupid_details '(("P21" "sex or gender" "Q6581097" "male")
                        ("P25" "mother" "Q47652" "Venus")
                        ("P460" "said to be the same as" "Q121973" "Eros")
                        ("P31" "instance of" "Q1470705" "fertility deity")
                        ("P31" "instance of" "Q2239243" "mythical creature")
                        ("P31" "instance of" "Q11688446" "Roman deity")))

(loop for line in cupid_details do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-wikidata)))
(loop for line in cupid_details do (inn-add-label-edge-from-nodes-label "Cupid" (fourth line) (second line)))

;; add eros details
;(setf *eros-details* (my-filter-query-results "Q121973"))
;(print *eros-details*)
(setf eros_details '(("P21" "sex or gender" "Q6581097" "male") 
                     ("P31" "instance of" "Q22989102" "Greek deity") 
                     ("P25" "mother" "Q35500" "Aphrodite") 
                     ("P31" "instance of" "Q1470705" "fertility deity") 
                     ("P5986" "Getty Iconography Authority ID" "901000613" "901000613")))

(loop for line in eros_details do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-wikidata)))
(loop for line in eros_details do (inn-add-label-edge-from-nodes-label "Eros" (fourth line) (second line)))


;; epithalamium Q686027
(defvar epithalamium_details '(("P279" "subclass of" "Q12119802" "lyric poetry genre")
                               ("P31" "instance of" "Q5185279" "poem")))


(loop for line in epithalamium_details do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-wikidata)))
(loop for line in epithalamium_details do (inn-add-label-edge-from-nodes-label "epithalamium" (fourth line) (second line)))



;; "Q3374376"	"mythological painting"
(defvar mythological_details '(("P31" "instance of" "Q16743958" "genre of painting")
                               ("P279" "subclass of" "Q115574326" "mythological art")
                               ("P941" "inspired by" "Q9134" "mythology")))

(loop for line in mythological_details do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-wikidata)))
(loop for line in mythological_details do (inn-add-label-edge-from-nodes-label "mythological painting" (fourth line) (second line)))


;; trunk query: my-filter-query-results "Q193472"

(inn-add-node *lotto-inn* (make-inn-node :label "tree" :type :kb-wikidata))
(inn-add-label-edge-from-nodes-label "trunk" "tree" "part of")




;;;;;;;;;;;;;;;
;; HyperReal ;;
;;;;;;;;;;;;;;;


;; look for possible symbols in which the depicted subjects may be included. 
; 1 look for the desired id
;(kb-sparql-query (format nil  sparql-query-symbol-id "Venus"))
; 2 look for symbolical meanings and contexts of the selected symbol id
;(kb-sparql-query (format nil  sparql-what-symbol-meaning-context "aphroditeVenus"))
; since there are many possible meanings, we selected few of them which are plausible

(setf venus_symbols '(("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-casualOrIllicitSex" "Simulation of casual or illicit sex in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-chastity" "Simulation of chastity in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-fertility" "Relatedness Simulation of fertility in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-loveInThePhysicalOrSexualSense" "Simulation of love in the physical or sexual sense in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-sexualPerversion" "Simulation of sexual perversion in a Greco-Roman context as Aphrodite/Venus" 
                        )))

(loop for line in venus_symbols do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-symbol)))
(loop for line in venus_symbols do (inn-add-label-edge-from-nodes-label "Venus" (fourth line) "has symbol"))


; conch
;(kb-sparql-query (format nil  *search-symbol-id* "conch"))
;(kb-sparql-query (format nil  *search-symbol-meaning-context* "conch"))

(setf conch_symbols '(("isSimulacrumOf" "is simulacrum of" "conch-aphroditeVenus" "Attribute Simulation of Aphrodite/Venus in a Greco-Roman context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-deitiesAssociatedWithTheSea" "Attribute Simulation of deities associated with the sea in a General or Unknown context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-fertility" "Association Simulation of fertility in a General or Unknown context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-gestation" "Association Simulation of gestation in a General or Unknown context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-theFemininePrinciple" "Simulation of the feminine principle in a General or Unknown Context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-vulva" "Simulation of vulva in a General or Unknown Context as conch")))


(loop for line in conch_symbols do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-symbol)))
(loop for line in conch_symbols do (inn-add-label-edge-from-nodes-label "conch" (fourth line) "has symbol"))

; tree
;(kb-sparql-query (format nil  *search-symbol-id* "tree"))
;(kb-sparql-query (format nil  *search-symbol-meaning-context* "tree"))

(setf tree_symbols '(("isSimulacrumOf" "is simulacrum of" "tree-fertility" "Simulation of fertility in a General or Unknown Context as tree")
("isSimulacrumOf" "is simulacrum of" "tree-life" "Simulation of life in a heraldic context as tree")))


(loop for line in tree_symbols do (inn-add-node *lotto-inn* (make-inn-node :label (fourth line) :type :kb-symbol)))
(loop for line in tree_symbols do (inn-add-label-edge-from-nodes-label "tree" (fourth line) "has symbol"))

)
