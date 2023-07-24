(ql:quickload :art-network)
(in-package :art-network)
(wi::activate-vis-js)

(wi::vis-destroy-network)

;; to do: create a function formatting nodes and edges from a list to be integrated with the make vis network function

; step 1: artwork metadata
; _____________________________________________________________________________________________________________


(progn
  (wi::add-element 
   `((div :id "lotto-metadata")
     ,(wi::make-vis-network 
       :element-id "lotto-metadata"
       :nodes (list "{id:'Q4009580', 
                      label:'Lorenzo Lotto, Venus and Cupid', 
                      shape: 'circularImage', 
                      image:'https://images.metmuseum.org/CRDImages/ep/original/DP-25975-001.jpg', 
                      color:'#C2FABC'}"
                    (format-node-color "title-venus-cupid" "Venus and Cupid" "manual")
                    (format-node-color "Q310973" "Lorenzo Lotto" "textrazor")
                    (format-node-color "Q174705" "oil painting" "textrazor")
                    (format-node-color "Q3305213" "painting" "kb-wikidata")
                    (format-node-color "Q641" "Venice" "textrazor")
                    (format-node-color "Q124110" "Loreto" "textrazor")
                    (format-node-color "Q5075601" "Charles Bierer Wrightsman" "textrazor")
                    )
       ;; since we have 2 cloths, we have to create two individuals linked to the wikidata entity
       :edges (list (format-edge-color "Q4009580" "title-venus-cupid" "title" "manual")
                    (format-edge-color "Q4009580" "Q310973" "creator" "kb-wikidata")
                    (format-edge-color  "Q4009580" "Q174705" "medium" "manual")
                    (format-edge-color "Q4009580" "Q3305213" "instance of" "kb-wikidata")
                    (format-edge-color "Q4009580" "Q5075601" "gifted by" "manual")

                    ))))
;; TextRazor correctly identified the painting in wikidata
;; Q4009580 link to query: https://w.wiki/6PvK  

(setq painting_details '(("P571" "inception" "1530"	"1530")
                  ("P180"	"depicts"	 "Q10791"	"nudity")
                  ("P180"	"depicts"	 "Q26771"	"Hedera")
                  ("P180"	"depicts"	 "Q47652"	"Venus")
                  ("P180"	"depicts"	 "Q5011"	"Cupid")
                  ("P180"	"depicts"	 "Q2102"	"snake")
                  ("P180"	"depicts"	 "Q457251"	"urolagnia")
                  ("P180"	"depicts"	 "Q62792"	"conch")
                  ("P180"	"depicts"	 "Q572447"	"aspic viper")
                  ("P180"	"depicts"	 "Q467"	         "woman")
                  ("P180"	"depicts"	 "Q487"	        "smile")
                  ("P180"	"depicts"	 "Q40446"	 "nude")
                  ("P180"	"depicts" 	"Q102231"	"rose")
                  ("P180"	"depicts" 	"Q643352"	"grass")
                  ("P180"	"depicts" 	"Q1397337"	"fascia")
                  ("P180"	"depicts" 	"Q1986098"	"standing")
                  ("P180"	"depicts" 	"Q6497446"	"veil")
                  ("P180"	"depicts" 	"Q6918773"	"ribbon")
                  ("P180"	"depicts" 	"Q250902"	"swagger stick")
                  ("P180"	"depicts" 	"Q746591"	"diadem")
                  ("P180"	"depicts" 	"Q17013150"	"switch")
                  ("P180"	"depicts" 	"Q193472"	"trunk")
                  ("P180"	"depicts" 	"Q188641"	"nipple")
                  ("P180"	"depicts" 	"Q843533"	"areola")
                  ("P180"	"depicts" 	"Q107412"	"petal")
                  ("P180"	"depicts" 	"Q213096"	"seashell")
                  ("P180"	"depicts" 	"Q1264081"	"censer")
                  ("P180"	"depicts" 	"Q3478442"	"half reclining")
                  ("P180"	"depicts" 	"Q15719234"	"bird's wing")
                  ("P180"	"depicts" 	"Q168456"	"earring")
                  ("P180"	"depicts" 	"Q201664"	"bracelet")
                  ("P180"	"depicts" 	"Q734844"	"laurel wreath")
                  ("P180"	"depicts" 	"Q105726"	"urination")
                  ("P180"	"depicts" 	"Q1544759"	"pubic hair removal")
                  ("P180"	"depicts" 	"Q3039121"	"drapery")
                  ("P180"	"depicts" 	"Q9103"	"breast")
                  ("P180"	"depicts" 	"Q43801"	"navel")
                 ; ("P170"	"creator"	 "Q310973"	"Lorenzo Lotto")
                  ("P136"	"genre"	         "Q40446"	"nude")
                  ("P195"	"collection"	 "Q160236"	"Metropolitan Museum of Art")
                  ("P144"	"based on"	 "Q686027"	"epithalamium")
                  ("P186"	"made from material"	 "Q296955"	"oil paint")
                  ("P186"	"made from material"	 "Q12321255"	"canvas")
                  ; ("P31"	"instance of"	 "Q3305213"	"painting")
                  ("P136"	"genre"	       "Q3374376"	"mythological painting")))

(add-info-to-node-color "Q4009580" painting_details "kb-wikidata")

;; add Lotto details
(defvar lotto_details '(("wdt:P19"	"place of birth"	 "Q641"	"Venice")
                        ("wdt:P20"	"place of death"	 "Q124110" "Loreto")
                  ("wdt:P569"	"date of birth"	"1480" "1480")
                  ("wdt:P570"	"date of death"	"1556" "1556")
                  ("wdt:P135"	"movement"	 "Q131808"	"mannerism")
                  ("wdt:P135"	"movement"	 "Q1474884"	"High Renaissance")
                  ("wdt:P937"	"work location"	 "Q641"	"Venice")
                  ("wdt:P937"	"work location" "Q220"	"Rome")
                  ("wdt:P937"	"work location" "Q3415"	"Ancona")
                  ("wdt:P937"	"work location" "Q5475"	"Treviso")))

(add-info-to-node-color "Q310973" lotto_details "kb-wikidata")


;; add medium details
(defvar medium_details '(("P186" "made from material" "Q161179" "pigment")
                         ("P279" "subclass of" "Q3300034" "painting material")
                         ("P279" "subclass of" "Q174219" "paint")))

(add-info-to-node-color "Q296955" medium_details "kb-wikidata")

;(setf *canvas-details* (my-filter-query-results "Q12321255"))
(defvar canvas_details '(("P279" "subclass of" "Q861259" "painting support")))

(add-info-to-node-color "Q12321255" canvas_details "kb-wikidata")

;; as there are many results for depict, we do not iterate new knowledge right now, but only when these subjects will be cited by the text

;; we focus on the characters in the title (Venus and Cupid)

;; add Venus details

(defvar venus_details '(("P21" "sex or gender" "Q6581072" "female")
                        ("P40" "child" "Q5011" "Cupid")
                        ("P31" "instance of" "Q205985" "goddess")
                        ("P31" "instance of" "Q1470705" "fertility deity")
                        ("P31" "instance of" "Q11688446" "Roman deity")
                        ("P460" "said to be the same as" "Q35500" "Aphrodite")
                        ("P1256" "Iconclass notation" "92C4" "92C4")))

(add-info-to-node-color "Q47652" venus_details "kb-wikidata")

;; retrieve more details for Afrodite

;(defparameter *afrodite-details* nil)
;(setf *afrodite-details* (my-filter-query-results "Q35500"))

(defvar afrodite_details  '(("P40" "child" "Q121973" "Eros") 
                            ("P361" "part of" "Q101609" "Twelve Olympians") 
                            ("P31" "instance of" "Q22989102" "Greek deity") 
                            ("P21" "sex or gender" "Q6581072" "female") 
                            ("P31" "instance of" "Q113103481" "Olympian god") 
                            ("P31" "instance of" "Q205985" "goddess") 
                            ("P31" "instance of" "Q1470705" "fertility deity") 
                            ("P2925" "domain of saint or deity" "Q7242" "beauty") 
                            ("P5986" "Getty Iconography Authority ID" "901000612" "901000612") 
                            ("P2925" "domain of saint or deity" "Q316" "love")))

(add-info-to-node-color "Q35500" afrodite_details "kb-wikidata")

;; add cupid details
;(setf *cupid-details* (my-filter-query-results "Q5011"))
(defvar cupid_details '(("P21" "sex or gender" "Q6581097" "male")
                        ("P25" "mother" "Q47652" "Venus")
                        ("P460" "said to be the same as" "Q121973" "Eros")
                        ("P31" "instance of" "Q1470705" "fertility deity")
                        ("P31" "instance of" "Q2239243" "mythical creature")
                        ("P31" "instance of" "Q11688446" "Roman deity")))

(add-info-to-node-color "Q5011" cupid_details "kb-wikidata")

;; add eros details
;(setf *eros-details* (my-filter-query-results "Q121973"))
;(print *eros-details*)
(setf eros_details '(("P21" "sex or gender" "Q6581097" "male") 
                     ("P31" "instance of" "Q22989102" "Greek deity") 
                     ("P25" "mother" "Q35500" "Aphrodite") 
                     ("P31" "instance of" "Q1470705" "fertility deity") 
                     ("P5986" "Getty Iconography Authority ID" "901000613" "901000613")))
(add-info-to-node-color "Q121973" eros_details "kb-wikidata")


;; epithalamium Q686027
(defvar epithalamium_details '(("P279" "subclass of" "Q12119802" "lyric poetry genre")
                               ("P31" "instance of" "Q5185279" "poem")))


(add-info-to-node-color "Q686027" epithalamium_details "kb-wikidata")

;; "Q3374376"	"mythological painting"

(defvar mythological_details '(("P31" "instance of" "Q16743958" "genre of painting")
                               ("P279" "subclass of" "Q115574326" "mythological art")
                               ("P941" "inspired by" "Q9134" "mythology")))


(add-info-to-node-color "Q3374376" mythological_details "kb-wikidata")

)
;; First phrase
;; ----------------------------------------------------------------------
(progn

;; text:" The picture shows Venus, the goddess of love, reclining naked on a cloth spread before a tree from which has been suspended a red cloth and a conch shell."

(setf initial-phrase1 '(("Q10884" "tree" "textrazor")
                        ("naked" "naked" "kb-manual")
                        ("red-cloth" "red cloth" "kb-manual")
                        ("spread-cloth" "spread cloth" "kb-manual")
                        ("red" "red" "kb-manual")
                        ("Q5849500" "cloth" "kb-manual") ;; wd but also wordnet
                        ("spread" "spread" "kb-manual")
                        ))

(add-formatted-nodes-color initial-phrase1)
;colors update
; Venus has been detected by textrazor
(vis-network-interface "nodes" "update" (format-node-color "Q47652" "Venus" "textrazor"))

; the following words are cited by the text but were already retrieved by wikidata in the previous phase: they turn pink
(vis-network-interface "nodes" "update" (format-node-color  "Q205985" "goddess" "kb-text"))
(vis-network-interface "nodes" "update" (format-node-color  "Q316" "love" "kb-text"))
(vis-network-interface "nodes" "update" (format-node-color   "Q62792"	"conch" "kb-text"))

;; retrieve more info about the newly discovered entities

;; in wordnet, naked is a synonim of nude. We link the two words
; (setf label_string -> to do: string with all the labels of the graph to detect possible synonyms
; (wn-synonym-search *label_string*)
(vis-network-interface "edges" "add"  (format-edge-color   "naked" "Q40446"	"wn synonyms" "wn"))

;; tree query 
;(setf *tree-details* (my-filter-query-results "Q10884"))
(setf tree_details '(("P279" "subclass of" "Q757163" "woody plant") ("P2670" "has part(s) of the class" "Q193472" "trunk")))
(add-info-to-node-color "Q10884" tree_details "kb-wikidata")


;; cloth query
;(setf *cloth-details* (my-filter-query-results "Q5849500"))
(setf cloth_details '(("P279" "subclass of" "Q28823" "textile")))
(add-info-to-node-color "Q5849500" cloth_details "kb-wikidata")


;; look for symbols 
; 1 look for the desired id
;(kb-sparql-query (format nil  *search-symbol-id* "Venus"))
; 2 look for symbolical meanings and contexts of the selected symbol id
;(kb-sparql-query (format nil  *search-symbol-meaning-context* "aphroditeVenus"))

(setf venus_symbols '(("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-casualOrIllicitSex" "Simulation of casual or illicit sex in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-chastity" "Simulation of chastity in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-fertility" "Relatedness Simulation of fertility in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-loveInThePhysicalOrSexualSense" "Simulation of love in the physical or sexual sense in a Greco-Roman context as Aphrodite/Venus")
                        ("isSimulacrumOf" "is simulacrum of" "aphroditeVenus-sexualPerversion" "Simulation of sexual perversion in a Greco-Roman context as Aphrodite/Venus" 
                        )))


(add-info-to-node-color "Q47652" venus_symbols "kb-symbol")

; conch
;(kb-sparql-query (format nil  *search-symbol-id* "conch"))
;(kb-sparql-query (format nil  *search-symbol-meaning-context* "conch"))

(setf conch_symbols '(("isSimulacrumOf" "is simulacrum of" "conch-aphroditeVenus" "Attribute Simulation of Aphrodite/Venus in a Greco-Roman context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-deitiesAssociatedWithTheSea" "Attribute Simulation of deities associated with the sea in a General or Unknown context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-fertility" "Association Simulation of fertility in a General or Unknown context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-gestation" "Association Simulation of gestation in a General or Unknown context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-theFemininePrinciple" "Simulation of the feminine principle in a General or Unknown Context as conch")
                      ("isSimulacrumOf" "is simulacrum of" "conch-vulva" "Simulation of vulva in a General or Unknown Context as conch")))


(add-info-to-node-color "Q62792" conch_symbols "kb-symbol")

; tree
;(kb-sparql-query (format nil  *search-symbol-id* "tree"))
;(kb-sparql-query (format nil  *search-symbol-meaning-context* "tree"))

(setf tree_symbols '(("isSimulacrumOf" "is simulacrum of" "tree-fertility" "Simulation of fertility in a General or Unknown Context as tree")
("isSimulacrumOf" "is simulacrum of" "tree-life" "Simulation of life in a heraldic context as tree")))
(add-info-to-node-color "Q10884" tree_symbols "kb-symbol")



; the artwork is present also in icon dataset. we retrieve results for it
; queries in knowledge graph > sparql

(setf *level1_subjects* '(("belt" "belt")
                          ("bow" "bow")
                          ("earring" "earring")
                          ("knotted-ribbon-bracelet" "knotted ribbon bracelet")
                          ("peeing-through-a-wreath" "peeing through a wreath")
                          ("putto" "putto")
                          ("rose" "rose")
                          ("rose-petal" "rose petal")
                          ("tiara" "tiara")
                          ("touching-breast" "touching breast")
                          ("wings" "wings")
                          ("woman" "woman")
                          ("holding-a-wreath" "holding a wreath")
                          ("myrtle-wreath" "myrtle wreath")
                          ("veil" "veil")
                          ("playful-attitude" "playful attitude")
                          ("cloth" "cloth")
                          ("cone-shell" "cone shell")
                          ("incense-burner" "incense burner")
                          ("ivy" "ivy")
                          ("rod" "rod")
                          ("serpent" "serpent")))

(setf *level1_subjects_nodes* (copy-object *level1_subjects*))

(add-to-end-list *level1_subjects_nodes* "kb-icon")
(print *level1_subjects_nodes*)
(add-formatted-nodes-color *level1_subjects_nodes*)

(setf *level1_subjects_edges* (copy-object *level1_subjects*))

(setf *level1_subjects_edges2* (add-to-beginning-list *level1_subjects_edges* "recognized level 1 subject"))
(setf *level1_subjects_edges3* (add-to-beginning-list *level1_subjects_edges2* "recognized-level-1-subject"))

(add-info-to-node-color "Q4009580" *level1_subjects_edges3* "manual")

;; add level 2 subjects

(setf *level2-subjects* '(("venus-belt" "Venus' belt")
                          ("incense-burner-bridal-chamber-decoration" "incense burner-bridal chamber decoration")
                          ("knotted-ribbon-bracelets-love" "knotted ribbon bracelets-love")
                          ("myrtle-wreath-conjugal-love" "myrtle wreath-conjugal love")
                          ("red-cloth-bridal-chamber-decoration" "red cloth-bridal chamber decoration")
                          ("venus-belt-seduction" "Venus' belt-seduction")
                          ("cone-shell-fertility" "cone shell-fertility")
                          ("ivy-eternal-love" "ivy-eternal love")
                          ("rod-venus-punished-cupid" "rod-venus punished Cupid")
                          ("serpent-pitfalls-of-love" "serpent-pitfalls of love")
                          ("cupid" "Cupid")
                          ("puer-mingens" "puer mingens")
                          ("rose-virginity" "rose-virginity")
                          ("playful-attitude-joy-love" "playful attitude-joy, love")
                          ("venus" "Venus")
                          ("venetian-bridal-clothes" "venetian bridal clothes")
                          ("urinating-wish-of-fertility" "urinating-wish of fertility")
                          ("puer-mingens-fertility" "puer mingens-fertility")
                          ("flower-of-venus" "flower of Venus")
                          ("decoration-for-wedding-occasions" "decoration for wedding occasions")))

(setf *level2_subjects_nodes* (copy-object *level2-subjects*))

(setf *level2_subjects_nodes2* (add-to-end-list *level2_subjects_nodes* "kb-icon"))
(add-formatted-nodes-color *level2_subjects_nodes2*)

;; link between level 1 and level 2 subjects 

(setf *level2-to-level1* '(("venus-belt" "belt")
                           ("incense-burner-bridal-chamber-decoration" "incense-burner")
                           ("knotted-ribbon-bracelets-love" "knotted-ribbon-bracelet")
                           ("myrtle-wreath-conjugal-love" "myrtle-wreath")
                           ("venus" "belt")
                           ("playful-attitude-joy-love" "bow")
                           ("venus" "earring")
                           ("venetian-bridal-clothes" "earring")
                           ("venus" "knotted-ribbon-bracelet")
                           ("urinating-wish-of-fertility" "peeing-through-a-wreath")
                           ("puer-mingens-fertility" "peeing-through-a-wreath")
                           ("urinating-wish-of-fertility" "putto")
                           ("playful-attitude-joy-love" "putto")
                           ("puer-mingens-fertility" "putto")
                           ("venus" "rose")
                           ("flower-of-venus" "rose")
                           ("decoration-for-wedding-occasions" "rose")
                           ("venus" "rose-petal")
                           ("flower-of-venus" "rose-petal")
                           ("decoration-for-wedding-occasions" "rose-petal")
                           ("venus" "tiara")
                           ("venetian-bridal-clothes" "tiara")
                           ("venus" "touching-breast")
                           ("playful-attitude-joy-love" "wings")
                           ("venus" "woman")
                           ("urinating-wish-of-fertility" "woman")
                           ("venus" "holding-a-wreath")
                           ("urinating-wish-of-fertility" "holding-a-wreath")
                           ("playful-attitude-joy-love" "holding-a-wreath")
                           ("puer-mingens-fertility" "holding-a-wreath")
                           ("urinating-wish-of-fertility" "myrtle-wreath")
                           ("venus" "veil")
                           ("venetian-bridal-clothes" "veil")
                           ("cupid" "bow")
                           ("puer-mingens" "peeing-through-a-wreath")
                           ("cupid" "putto")
                           ("puer-mingens putto")
                           ("rose-virginity" "rose")
                           ("rose-virginity" "rose-petal")
                           ("cupid" "wings")
                           ("cupid" "holding-a-wreath")
                           ("puer-mingens" "holding-a-wreath")
                           ("playful-attitude-joy-love" "playful-attitude")
                           ("cupid" "playful-attitude")
                           ("red-cloth-bridal-chamber-decoration" "cloth")
                           ("venus-belt-seduction" "belt")
                           ("cone-shell-fertility" "cone-shell")
                           ("ivy-eternal-love" "ivy")
                           ("rod-venus-punished-cupid" "rod")
                           ("serpent-pitfalls-of-love" "serpent")))

(setf *level2-to-level1-edges* (copy-object *level2-to-level1*))
(setf *level2-to-level1-edges2* (add-to-end-list *level2-to-level1-edges* "composed of"))
(setf *level2-to-level1-edges3* (add-to-end-list *level2-to-level1-edges2* "manual"))
(add-formatted-edges-color *level2-to-level1-edges3*)

;; third level meaning 
(add-formatted-nodes-color '(("wedding-wish" "wedding wish" "kb-icon")))
(add-formatted-edges-color '(("Q4009580" "wedding-wish" "has third level meaning" "manual")))


;; find duplicate nodes and link them
; to do: extract labels list
;(detect-duplicates *labels-list*)

; test run on python for the Wikidata depicted subjects and icon dataset subjects
; result of the automatic duplicates detection: ['earring', 'rose', 'woman', 'veil', 'Venus', 'Cupid'] 

; to do: function to retrieve nodes id from the label string. Here we simulate it manually 

(setf *same-as-adges* '(("earring" "Q168456" "same as" "automatic")
                        ("rose" "Q102231" "same as" "automatic")
                        ("woman" "Q467" "same as" "automatic")
                        ("veil" "Q6497446" "same as" "automatic")
                        ("venus" "Q47652" "same as" "automatic")
                        ("cupid" "Q5011" "same as" "automatic")))

(add-formatted-edges-color *same-as-adges*)
; align through wordnet

(add-formatted-edges-color '(("serpent" "Q2102" "has synonym" "wn")))

; add manually aligned nodes

(setf *same-as-adges-manual* '(("cone-shell" "Q213096" "same as" "manual") ; cone shell and sea shell
                        ("cloth" "Q3039121" "same as" "manual") ; cloth and drapery
                        ("rose-petal" "Q107412" "same as" "manual") ; rose petal and petal
                        ("belt" "Q1397337" "same as" "manual") ; belt and fascia
                        ("wings" "Q15719234" "same as" "manual") ; wings and bird's wing
                        ("tiara" "Q746591" "same as" "manual") ; diadem and tiara
                        ("rod" "Q250902" "same as" "manual") ; rod and swagger stick
                        )) 

(add-formatted-edges-color *same-as-adges-manual*)

)


