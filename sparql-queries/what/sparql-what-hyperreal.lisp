
(in-package :art-network)


(defun sparql-what-symbol-meaning-context (symbol-id   &optional (kg :icon-dataset))
 "query to search by id the symbol and the context of symbols, i.e. in which cultural context they have which symbolical meaning "

  (format nil "
       prefix kb: <https://w3id.org/simulation/data/> 
       prefix sim: <https://w3id.org/simulation/ontology/> 

       SELECT DISTINCT ?symbol ?symbolLabel ?contextLabel WHERE {
            kb:~a sim:isSimulacrumOf ?symbol.
            ?symbol rdfs:label ?symbolLabel. 
            OPTIONAL {?symbol sim:hasContext ?context. ?context rdfs:label ?contextLabel}
                
       }" symbol-id))
; (sparql-what-symbol-meaning-context "aphroditeVenus")