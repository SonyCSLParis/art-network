
(in-package :art-network)

(unless cl-wikimedia::*user-agent*
  (setf cl-wikimedia::*user-agent* "unit-test-bot"))

(deftest test-wikidata ()
    (let* ((title "Venus and Cupid")
           (creator "Lorenzo Lotto")
           (language "en")
           (response (wikidata-sparql-query (sparql-query-artwork title creator language)))
           (entity-id (extract-id-from-uri (follow-path-to-item response))))
      (test-assert (string= "Q4009580" entity-id))))
;; (test-wikidata)
