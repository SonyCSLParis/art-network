;; Copyright Sony Computer Science Laboratory - Paris
;;           Sofia Baroncini
;;           Luc Steels
;;           Remi van Trijp

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :art-network)

;;; #############################
;;; ### Knowledge Base Server ###
;;; #############################

(defparameter *kb-server* nil "Set this parameter to the address of the server that hosts the knowledge base.")
;; Assuming that the knowledge base is run on a local server using Flask in python:
(setf *kb-server* "http://127.0.0.1:5000")

;;; #####################################
;;; ### Making requests to the server ###
;;; #####################################

(defun send-kb-server-request (route parameters &key (host *kb-server*) (method :get)
                                     (content-type "application/json") (connection-timeout 20))
  "Main function for making requests."
  (let* ((url (string-append host route))
         (json (json:encode-json-to-string parameters))
         (response (drakma:http-request url
                                        :method method
                                        :content-type content-type
                                        :content json
                                        :connection-timeout connection-timeout)))
    (when response (handler-case (cl-json:decode-json-from-string response)
                     (error (e)
                       (format t "Error in response from the kb-server: ~S.~&" e))))))

(defun kb-sparql-query (sparql-query &key (host *kb-server*) (connection-timeout 20))
  "Send a sparql-query to the knowledge base server."
  (send-kb-server-request "/query"
                          `((:query . ,sparql-query))
                          :host host :method :get
                          :content-type "application/json"
                          :connection-timeout connection-timeout))

#| 
Example (make sure your python app is running before trying)
------------------------------------------------------------
(defparameter *test-query* (format nil "prefix kb: <https://w3id.org/simulation/data/> 
                                        prefix sim: <https://w3id.org/simulation/ontology/> 

                                        SELECT ?symbol ?context WHERE {
                                           kb:aphroditeVenus sim:isSimulacrumOf ?symbol.
                                        OPTIONAL {?symbol sim:hasContext ?context}

                                        }"))

 Query to retrieve the symbols looking for a keyword in the label - problem: it runs out of time


(defparameter *search-symbol-label*  "prefix kb: <https://w3id.org/simulation/data/> 
                                      prefix sim: <https://w3id.org/simulation/ontology/> 

                                      SELECT DISTINCT ?symbol ?context WHERE {
                                      ?sim sim:isSimulacrumOf ?symbol; rdfs:label ?simLabel. 

                                      FILTER(regex(str(?simLabel), ('~a'), 'i'))  
                                       OPTIONAL {?symbol sim:hasContext ?context}

                                                 }")


(kb-sparql-query *test-query*)
(kb-sparql-query (format nil  *search-symbol-label* "Venus"))
|#

(defun wd-id-det (wdid &key (host *kb-server*))
  "Ask to retrieve the details of a given wd entity"
  (send-kb-server-request "/wd_id_details"
                          `((:wdid . ,wdid))
                          :host host :method :get
                          :content-type "application/json"
                          ))

#|

|#

#| 
Example (make sure your python app is running before trying)
------------------------------------------------------------

(wd-id-det "Q12321255")
|#

(defun wn-synonym-search (wn-list &key (host *kb-server*))
  "Ask to identify the wordnet synonyms from the input of a string of labels separated by space"
  (send-kb-server-request "/wn_synonyms"
                          `((:wnlist . ,wn-list))
                          :host host :method :get
                          :content-type "application/json"
                          ))

; test
; (setf *example_list* (list "nude" "Venus" "Cupid" "naked" "gentle" "soft"))
; (setf *example_string* "nude Venus Cupid naked gentle soft")
; (wn-synonym-search *example_string*)
; (print example_list)