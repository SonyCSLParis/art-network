(ql:quickload :art-network)
;; (ql:quickload :art-network)
;; (in-package :art-network)

;; (ql:quickload :web-interface)
;; (load "systems/web-interface/vis-js.lisp")
(wi::activate-vis-js)


(wi::vis-destroy-network)

;



;; phrase analyzed: The picture shows Venus, the goddess of love, reclining naked on a cloth spread before a tree from which has been suspended a red cloth and a conch shell.


(wi::add-element `((div :id "LottoPhrase1Step1")
               ,(wi::make-vis-network :element-id "LottoPhrase1Step1"
                                  :nodes (list "{id:'OBJ1002test', label:'Lorenzo Lotto, Venus and Cupid', shape: 'circularImage', image:'https://images.metmuseum.org/CRDImages/ep/original/DP-25975-001.jpg'}"
                                           ;(format-node "Q47652" "Venus")
                                           ;;"{id: 'Q47652', label:'Venus', color:'#C2FABC'}"
                                           "{id: 'Q10884', label:'Tree', color:'#C2FABC'}"
                                           "{id: 'Q205985', label:'goddess'}"
                                           "{id: 'love', label:'love'}"
                                           "{id: 'reclining-wn', label:'reclining'}"
                                           "{id:'naked-wn', label:'naked'}"
                                           "{id:'spread-cloth', label:'spread cloth'}"
                                           "{id:'Q5849500', label:'cloth'}"
                                           "{id:'spread-wn', label:'spread'}"
                                           "{id:'red-wn', label:'red'}"
                                           "{id:'red-cloth', label:'red cloth'}"
                                           "{id:'Q62792', label:'conch'}")
;; since we have 2 cloths, we have to create two individuals linked to the wikidata entity
                                           :edges (list ;(format-edge "spread-cloth" "Q5849500" "type")
                                                        ;(format-edge "red-cloth" "Q5849500" "type")
                                                    "{from: 'spread-cloth', to: 'Q5849500', label: 'type'}"
                                           "{from: 'red-cloth', to: 'Q5849500', label: 'type'}"
                                                        ))))
;; test
;(wi::vis-add-node "{id: 'lorenzo-lotto', label: 'Lorenzo Lotto'}")

;(defvar lotto_details '(("wdt:P19"	"place of birth"	 "Q641"	"Venice")
;                  ("wdt:P569"	"date of birth"	"1480" "1 January 1480")
;                  ("wdt:P570"	"date of death"	"1556" "1 January 1556")
;                  ("wdt:P135"	"movement"	 "Q131808"	"mannerism")
;                  ("wdt:P135"	"movement"	 "Q1474884"	"High Renaissance")
;                  ("wdt:P937"	"work location"	 "Q641"	"Venice")
;                  ("wdt:P937"	"work location" "Q220"	"Rome")
;                  ("wdt:P937"	"work location" "Q3415"	"Ancona")
;                  ("wdt:P937"	"work location" "Q5475"	"Treviso")))

;(add-info-to-node "lorenzo-lotto" lotto_details)



;; step2: query wikidata 
;; query in the spreadsheet to retrieve all the relations from a given individual
(progn
;; venus query
(wi::vis-add-node "{id: 'Q5011', label: 'Cupid'}")
(wi::vis-add-node "{id: 'Q1470705', label: 'fertility deity'}")
(wi::vis-add-node "{id: 'Q11688446', label: 'roman deity'}")
(wi::vis-add-node "{id: 'Q35500', label: 'Aphrodite'}")

(wi::vis-add-edge "{from: 'Q47652', to: 'Q205985', label: 'instance of'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'Q5011', label: 'child'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'Q1470705', label: 'instance of'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'Q11688446', label: 'instance of'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'Q35500', label: 'said to be the same as'}")


;; tree query 
(wi::vis-add-node "{id: 'Q193472', label: 'trunk'}")
(wi::vis-add-edge "{from: 'Q10884', to: 'Q193472', label: 'has part(s) of the class'}")


;; cloth query
(wi::vis-add-node "{id: 'Q28823', label: 'textile'}")
(wi::vis-add-edge "{from: 'Q5849500', to: 'Q28823', label: 'subclass of'}")

;; retrieve knowledge about the newly discovered relevant entities (Cupid)
(wi::vis-add-node "{id: 'Q121973', label: 'Eros'}")
(wi::vis-add-edge "{from: 'Q5011', to: 'Q121973', label: 'said to be the same as'}")

;; retrieve knowledge about the newly discovered relevant entities (Eros)
(wi::vis-add-edge "{from: 'Q121973', to: 'Q1470705', label: 'instance of'}")

)
;; step 2: query HyperReal. turtle file: https://raw.githubusercontent.com/br0ast/simulationontology/main/KG/kg.ttl 


;; criteria of filtering: 
;; 1 - since the Painting is in western art context, for the moment we consider only symbols of contexts that are likely to be known by the artist, namely: general, graeco-roman, chiristian
;; we will search for a symbolic meaning for the major part of the object depicted as a concequence of background knowledge. 
;; Background knowledge: Italian XVI century is a period in which the intellectual elite loved to play with viewers and create sophisticated symbols to express motti, ideas. In addition, Lotto usually makes an intense use of symbols in his painting. So we are allowed to assume that we can find many symbols in the painting. 

;; 3- since we know from the title that venus and Cupid are central characters of the painting, we will retrieve possible simbolic meanings related to their area. 
;; 4 - since we know from the text that Venus is the goddess of love, and from Wikidata that she is the goddess of fertility, we will select hose symbols having these or related meanings (love, fertility)


;; link to the colab with queries: https://colab.research.google.com/drive/1iBOn-edfMT4CkDLKDKpCNIGfjQdf_ciD?usp=sharing 

;; symbols for Venus

;; 5- after the cited restrictions, some concepts were not added because they seemed far from the overall interpretation provided by the art historian, such as the concept of ritual prostitution, sexual perversion

(progn

(wi::vis-add-node "{id: 'aphroditeVenus-fertility', label: 'Venus related to fertility'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'aphroditeVenus-fertility', label: 'part of symbol'}")
(wi::vis-add-node "{id: 'grecoRoman', label: 'greco-roman context'}")
(wi::vis-add-edge "{from: 'venus-fertility', to: 'heraldic', label: 'has context'}")

(wi::vis-add-node "{id: 'aphroditeVenus-loveInThePhysicalOrSexualSense', label: 'Venus associated to love in the physical or sexual sense'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'aphroditeVenus-loveInThePhysicalOrSexualSense', label: 'part of symbol'}")
(wi::vis-add-edge "{from: 'aphroditeVenus-loveInThePhysicalOrSexualSense', to: 'grecoRoman', label: 'has context'}")

;; the following symbols are in contrast to each other: at a certain point of the interpretation we will have to choose which one to discard
(wi::vis-add-node "{id: 'aphroditeVenus-casualOrIllicitSex', label: 'Venus associated to casual or illicit sex'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'aphroditeVenus-casualOrIllicitSex', label: 'part of symbol'}")
(wi::vis-add-edge "{from: 'aphroditeVenus-casualOrIllicitSex', to: 'grecoRoman', label: 'has context'}")

(wi::vis-add-node "{id: 'aphroditeVenus-chastity', label: 'Venus associated to chastity'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'aphroditeVenus-chastity', label: 'part of symbol'}")
(wi::vis-add-edge "{from: 'aphroditeVenus-chastity', to: 'grecoRoman', label: 'has context'}")




;; symbols for conch
;; same criteria as Venus
(wi::vis-add-node "{id: 'conch-fertility', label: 'conch associated to fertility'}")
(wi::vis-add-edge "{from: 'Q62792', to: 'conch-fertility', label: 'part of symbol'}")

(wi::vis-add-node "{id: 'conch-gestation', label: 'conch associated to gestation'}")
(wi::vis-add-edge "{from: 'Q62792', to: 'conch-gestation', label: 'part of symbol'}")

(wi::vis-add-node "{id: 'conch-theFemininePrinciple', label: 'conch is symbol of the feminine principle'}")
(wi::vis-add-edge "{from: 'Q62792', to: 'conch-theFemininePrinciple', label: 'part of symbol'}")

(wi::vis-add-node "{id: 'conch-vulva', label: 'conch is symbol of vulva'}")
(wi::vis-add-edge "{from: 'Q62792', to: 'conch-vulva', label: 'part of symbol'}")


;; symbols for tree
;; same criteria (fertility and love) 

(wi::vis-add-node "{id: 'tree-fertility', label: 'tree is symbol of fertility'}")
(wi::vis-add-edge "{from: 'Q10884', to: 'tree-fertility', label: 'part of symbol'}")

(wi::vis-add-node "{id: 'tree-life', label: 'tree is symbol of life'}")
(wi::vis-add-node "{id: 'heraldic', label: 'heraldic context'}")
(wi::vis-add-edge "{from: 'Q10884', to: 'tree-life', label: 'part of symbol'}")
(wi::vis-add-edge "{from: 'tree-life', to: 'heraldic', label: 'has context'}")

;; symbols for Cupid - only one result

(wi::vis-add-node "{id: 'erosCupid-romanticLove', label: 'Cupid symbol of romantic love'}")
(wi::vis-add-edge "{from: 'Q5011', to: 'erosCupid-romanticLove', label: 'part of symbol'}")
(wi::vis-add-edge "{from: 'erosCupid-romanticLove', to: 'grecoRoman', label: 'has context'}")
)
;; step 3 - simulated syntactic analysis: we connect adjectives to nouns 

(progn
(wi::vis-add-edge "{from: 'red-cloth', to: 'red-wn', label: 'has adjective'}")
(wi::vis-add-edge "{from: 'spread-cloth', to: 'spread-wn', label: 'has adjective'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'reclining-wn', label: 'described as - adjective'}")
(wi::vis-add-edge "{from: 'Q47652', to: 'naked-wn', label: 'described as - adjective'}")

;; frame extracted: the picture shows - I added only Venus

(wi::vis-add-edge "{from: 'OBJ1002test', to: 'Q47652', label: 'shows'}")

;; frame: spread
(wi::vis-add-edge "{from: 'spread-cloth', to: 'Q10884', label: 'spread before'}")

;; frame extracted: suspended - I split the arguments
(wi::vis-add-edge "{from: 'Q62792', to: 'Q10884', label: 'suspended on'}")
(wi::vis-add-edge "{from: 'red-cloth', to: 'Q10884', label: 'suspended on'}")
)
;; are the new nouns with adjective also symbols? We go back to step 2 to verify it. We had no results. 

;; step 3 - resulting semantic frames - not sure about this part. 

;; possible symbols afer step 3: query HyperReal