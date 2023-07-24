(ql:quickload :art-network)
(in-package :art-network)
(wi::activate-vis-js)

(wi::vis-destroy-network)

;; to do: create a function formatting nodes and edges from a list to be integrated with the make vis network function

; step 1: artwork metadata
; _____________________________________________________________________________________________________________


(progn
(wi::add-element `((div :id "lotto-metadata")
               ,(wi::make-vis-network :element-id "lotto-metadata"
                                  :nodes (list "{id:'Q4009580', label:'Lorenzo Lotto, Venus and Cupid', shape: 'circularImage', image:'https://images.metmuseum.org/CRDImages/ep/original/DP-25975-001.jpg', color:'#C2FABC'}"
                                           (format-node-color "title-venus-cupid" "Venus and Cupid" "manual")
                                           (format-node-color "Q310973" "Lorenzo Lotto" "textrazor")
                                           (format-node-color "Q174705" "oil painting" "textrazor")
                                           (format-node-color "Q3305213" "painting" "kb-query")
                                           (format-node-color "Q641"	"Venice" "textrazor")
                                           (format-node-color "Q124110"	"Loreto" "textrazor")
                                           (format-node-color "Q5075601" "Charles Bierer Wrightsman" "textrazor")
)
;; since we have 2 cloths, we have to create two individuals linked to the wikidata entity
                                           :edges (list (format-edge-color "Q4009580" "title-venus-cupid" "title" "manual")
                                                        (format-edge-color "Q4009580" "Q310973" "creator" "kb-query")
                                                        (format-edge-color  "Q4009580" "Q174705" "medium" "manual")
                                                        (format-edge-color "Q4009580" "Q3305213" "instance of" "kb-query")
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

(add-info-to-node-color "Q4009580" painting_details "kb-query")

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

(add-info-to-node-color "Q310973" lotto_details "kb-query")


;; add medium details
(defvar medium_details '(("P186" "made from material" "Q161179" "pigment")
 ("P279" "subclass of" "Q3300034" "painting material")
 ("P279" "subclass of" "Q174219" "paint")))

(add-info-to-node-color "Q296955" medium_details "kb-query")

;(setf *canvas-details* (my-filter-query-results "Q12321255"))
(defvar canvas_details '(("P279" "subclass of" "Q861259" "painting support")))

(add-info-to-node-color "Q12321255" canvas_details "kb-query")

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

(add-info-to-node-color "Q47652" venus_details "kb-query")

;; retrieve more details for Afrodite

;(defparameter *afrodite-details* nil)
;(setf *afrodite-details* (my-filter-query-results "Q35500"))

(defvar afrodite_details  '(("P40" "child" "Q121973" "Eros") ("P361" "part of" "Q101609" "Twelve Olympians") ("P31" "instance of" "Q22989102" "Greek deity") ("P21" "sex or gender" "Q6581072" "female") ("P31" "instance of" "Q113103481" "Olympian god") ("P31" "instance of" "Q205985" "goddess") ("P31" "instance of" "Q1470705" "fertility deity") ("P2925" "domain of saint or deity" "Q7242" "beauty") ("P5986" "Getty Iconography Authority ID" "901000612" "901000612") ("P2925" "domain of saint or deity" "Q316" "love")))

(add-info-to-node-color "Q35500" afrodite_details "kb-query")

;; add cupid details
;(setf *cupid-details* (my-filter-query-results "Q5011"))
(defvar cupid_details '(("P21" "sex or gender" "Q6581097" "male")
 ("P25" "mother" "Q47652" "Venus")
 ("P460" "said to be the same as" "Q121973" "Eros")
 ("P31" "instance of" "Q1470705" "fertility deity")
 ("P31" "instance of" "Q2239243" "mythical creature")
 ("P31" "instance of" "Q11688446" "Roman deity")))

(add-info-to-node-color "Q5011" cupid_details "kb-query")

;; add eros details
;(setf *eros-details* (my-filter-query-results "Q121973"))
(print *eros-details*)
(setf eros_details '(("P21" "sex or gender" "Q6581097" "male") ("P31" "instance of" "Q22989102" "Greek deity") ("P25" "mother" "Q35500" "Aphrodite") ("P31" "instance of" "Q1470705" "fertility deity") ("P5986" "Getty Iconography Authority ID" "901000613" "901000613")))
(add-info-to-node-color "Q121973" eros_details "kb-query")


;; epithalamium Q686027
(defvar epithalamium_details '(("P279" "subclass of" "Q12119802" "lyric poetry genre")
                               ("P31" "instance of" "Q5185279" "poem")))


(add-info-to-node-color "Q686027" epithalamium_details "kb-query")

;; "Q3374376"	"mythological painting"

(defvar mythological_details '(("P31" "instance of" "Q16743958" "genre of painting")
 ("P279" "subclass of" "Q115574326" "mythological art")
 ("P941" "inspired by" "Q9134" "mythology")))


(add-info-to-node-color "Q3374376" mythological_details "kb-query")

)
;; First phrase
;; ----------------------------------------------------------------------


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
(vis-network-interface "edges" "add"  (format-edge-color   "naked" "Q40446"	"wn synonyms" "wn"))

;; tree query 
;(setf *tree-details* (my-filter-query-results "Q10884"))
(setf tree_details '(("P279" "subclass of" "Q757163" "woody plant") ("P2670" "has part(s) of the class" "Q193472" "trunk")))
(add-info-to-node-color "Q10884" tree_details "kb-query")


;; cloth query
(setf *cloth-details* (my-filter-query-results "Q5849500"))
(setf cloth_details '(("P279" "subclass of" "Q28823" "textile")))
(add-info-to-node-color "Q5849500" cloth_details "kb-query")


;; look for symbols 
(kb-sparql-query (format nil  *search-symbol-label* "Venus"))
