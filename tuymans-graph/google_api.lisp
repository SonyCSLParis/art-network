;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; HumaneAI-NET micro-project (November 2020 - March 2021) 
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;; Interfacing IRL with Knowledge Graph APIs                            ;;
                     ;; FCG Team : Luc, Remi, Martina                                        ;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;                              DRAFT                                   ;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This file explains step-by-step how to query an API to load Knowledge Graphs encoded in Json file
;; into Lisp List. The final goal will be building the bridge between the newly created Lisp List
;; and IRL.
;;
;; Content:                                         | What's discussed
;; -------------------------------------------------+-------------------------------------------
;; Part 1: Load the packages                        | Use Quicklisp to load the package
;; Part 2: Query an API Load the KG as a Lisp List  |
;;   Part 2a: How to Query Google's KG API          | Function to query Google API
;;   Part 2b: How to Query VUA's API                | Work in progress ... 
;; Part 3: Bridge with IRL                          | Work in progress ... 



;; #########################################################
;; Part 1. Load the packages needed
;; #########################################################

;; Use your quicklisp to load the packages :drakma and :yason 
;; Reference manual for Drakma: https://edicl.github.io/drakma/
;; Reference manual for Yason: https://quickref.common-lisp.net/yason.html

(ql:quickload '(:drakma :yason))

;; Use your quicklisp to load the package :irl

(ql:quickload :irl)
(in-package :irl)


;; #########################################################
;; Part 2. Query an API (Google, Spotify, VUA)
;; and decode the KG (a Json file) into Lisp as a list
;; #########################################################

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Part 2a : Query Google's Knowledge Graph API
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; The first thing you need to do to access Google's Knowledge Graph API is to get a Google API key.
;; How?
;; 1. Visit this page https://developers.google.com/knowledge-graph/prereq.
;; 2. Goes to the section "Create a project for your client"
;; 3. Click on the link "use the setup tool".
;; 4. Log in to your Google account
;; 5. Create a project and get a unique API Key on the "credentials section".
;; 6. Please note the "restrictions" section  -->  You can restrict the key before using it in production by
;;                                                 clicking the Restrict key and selecting one of the Restriction
;;                                                 To keep your API keys secure, follow the best practices at
;;                                                 this page https://cloud.google.com/docs/authentication/api-keys 
;; 6. Copy your key below and load it

(defparameter *api-key* nil "Set your own Google Knowledge Graph API key.")
(setf *api-key* "AIzaSyBMKvpqJPkJM7VeAz-d7Twx8RrVPYc5e6c") ;; Please insert here your API key

;; Below we define the function to query the Google Knowledge Graph API

;; Please Note: currently what follows is a specific function for querying Google's knowledge base
;; This means that this function won't work for querying for example the VUA's API.

;; #TODO : the code on this page is just a draft, what maybe will best to do is to see whether it's
;; possible to implement a unique function to query different APIs. 

(defun google-kg-api-search-request (&key (query "luc+steels") ;; Define your search query 
                                          (limit 1) ;; Set the maximum number of results you want to get
                                          (api-key *api-key*) ;; Load your API key
                                          (content-type "application/json")) ;; Define the content-type
  "Function to query the google knowledge graph API"
  (let* ((http-request (format nil "https://kgsearch.googleapis.com/v1/entities:search?query=~a&key=~a&limit=~a&indent=True&languages=en&types=person"
                               query api-key limit)) ;; define your http request 
         (stream (drakma:http-request http-request :method :get :content-type content-type 
                                      :want-stream t))) ;; use drakma and the REST method get to get the KG 
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8) 
    (yason:parse stream :object-as :alist))) ;; instead of :alist we can also choose :plist or :hashtable

;; Before testing the function please note several things :

;; First have a look at the http request you are about to send : https://kgsearch.googleapis.com/v1/entities:search?query=~a&key=~a&limit=~a&indent=True&languages=en&types=person
;; 1. https://kgsearch.googleapis.com/v1/entities:search? -- > this is the  minimal part you need to put when ;;
;;                                                             sending an HTPP request to the Google KG API
;; You can change the other parts (please note that they are separated by "&")
;; 2. query=~a --> define a character string to look for in Google Knowledge Graph. 
;; 3. key=~a --> Google API key.
;; 4. limit=~a --> A numeric value limiting the number of entities to be returned. The maximum is 500. Defaults to 20 
;;                 Please note that requests with high limits have a higher chance of timing out.
;; 5. indent=True --> A logical argument enabling indenting of JSON results. Defaults to NULL.
;; 6. languages=en --> A character argument defining the language filter. The list of language codes (defined
;;                    in ISO 639) to run the query with, for instance 'en', but it could be 'nl'.
;;                    Default to NULL.
;; 7. types=person --> A character argument restricting returned entities to those of the specified types. See
;;                     schema.org for valid types (e.g. 'Person' as defined in http://schema.org/Person restri
;;                     cts the results to entities representing people).
;;                     If multiple types are specified, returned entities will contain one or more of these types.
;;                    Defaults to NULL.  


;; If you are interested in digging into REST methods other than (:method :get) please check the section
;; "Learn Rest Basics" at https://developers.google.com/knowledge-graph/prereqs#learn-rest-basics


;; If you are interested in the different keys possible for the "http-request" method, please check the
;; The Drakma dictionary at https://edicl.github.io/drakma/#arg-want-stream at the section "Requests"


;; Now you are ready to test the function and to load Google's KG about Luc Steels as Lisp List ! 

;(setf *LS* (google-kg-api-search-request :query "luc+steels":api-key *api-key* :limit 1))
(setf *la-pelle* (google-kg-api-search-request :query "la+pelle":api-key *api-key* :limit 1))

(pprint *la-pelle*)