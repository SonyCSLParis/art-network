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

(defparameter *wikidata-properties* nil "Add property IDs that are of interest.")

;; Statements that are of interest to us.
;; -----------------------------------------------------------------------------------------------
(setf *wikidata-properties*
      (list '("P1476" "title")
            '("P170" "creator")
            '("P18" "image")
            '("P31" "instance of")
            '("P180" "depicts")
            '("P571" "inception")
            '("P276" "location")
            '("P144" "based on")
            '("P136" "genre")
            '("P941" "inspired by")
            '("P186" "made from material")))

(defun print-interesting-statements (&optional (properties *wikidata-properties*))
  (pprint properties))

;; Some helper functions
;; ----------------------------------------------------------------------------------------
(defun follow-path-to-item (http-response &optional (item "uri"))
  "Helper function to extract what we want."
  (gethash "value" 
           (gethash item (first (gethash "bindings" (gethash "results" http-response))))))

(defun wikidata-query-results (http-response)
  (gethash "bindings" (gethash "results" http-response)))

(defun extract-id-from-uri (uri)
  "Get the last part of a uri after the slash."
  (reverse (first (split-string (reverse uri) "/"))))

(defun get-wikidata-entity-id (entity-data)
  (assert (hash-table-p entity-data))
  (gethash "id" entity-data))

(defun get-statement-id (wikidata-number entity-data)
  "Get the statement ID of a (list of) statement(s) if it/they exist(s)."
  (let ((statements (gethash wikidata-number (gethash "statements" entity-data))))
    (cond ((null statements) nil)
          ((null (rest statements)) (gethash "id" (first statements)))
          (t
           (mapcar #'(lambda(statement)
                       (gethash "id" statement))
                   statements)))))

(defun get-entity-description (entity-data &optional (language "en"))
  "Given entity-data from wikidata, get its description (if exists)"
  (let ((descriptions (gethash "descriptions" entity-data)))
    (if descriptions
      (gethash language descriptions)
      nil)))

(defun get-entity-label (entity-data &optional (language "en"))
  "Given entity-data from wikidata, find its label (if exists)"
  (let ((labels (gethash "labels" entity-data)))
    (if labels
      (gethash language labels)
      nil)))

;; Accessing wikidata information
;; ------------------------------------------------------------------------------------------
(defmethod an-get-artwork-data ((knowledge-graph (eql :wikidata))
                                &key id title creator language (lisp-format :hash-table)
                                &allow-other-keys)
  (let* ((language-code (or language "en"))
         (entity-id (or id
                        (extract-id-from-uri 
                         (follow-path-to-item 
                          (wikidata-sparql-query (sparql-query-artwork title creator language-code)))))))
    (wikidata-get-entity entity-id :lisp-format lisp-format)))
;; (an-get-artwork-data :wikidata :title "Venus and Cupid" :creator "Lorenzo Lotto" :lisp-format :alist)

(defmethod an-get-artwork-depicts ((knowledge-graph (eql :wikidata))
                                   &key id (language "en")
                                   &allow-other-keys)
  (let* ((sparql-query (sparql-query-depicts id language))
         (results (gethash "bindings" (gethash "results" (wikidata-sparql-query sparql-query)))))
    (loop for result in results
          for label = (gethash "value" (gethash "label" result))
          for identifier = (gethash "value" (gethash "identifier" result))
          collect (list label identifier))))
;; (an-get-artwork-depicts :wikidata :id "Q4009580")

;; -----------------------------------------------------------------------------------------
;; Get Wikidata properties: methods that return the requested information + the statement ID
;; -----------------------------------------------------------------------------------------

(defgeneric wikidata-property (artwork-data property &key &allow-other-keys))

(defmethod wikidata-property (artwork-data property &key &allow-other-keys)
  (error (format nil "Please implement a wikidata-property method for property ~a" property)))

;; Get the title (if it exists).
;; --------------------------------------------------------------------------------------------------
(defmethod wikidata-property ((artwork-data hash-table)
                              (property (eql :title))
                              &key
                              &allow-other-keys)
  (let ((statement-id (get-statement-id "P1476" artwork-data)))
    (if statement-id
      (let* ((statement-data (wikidata-get-statement statement-id :lisp-format :hash-table))
             (title (gethash "text" (gethash "content" (gethash "value" statement-data)))))
        (values title statement-id))
      nil)))
; (setf *artwork-data* (an-get-artwork-data :wikidata :id "Q4009580"))
; (wikidata-property *artwork-data* :title)


;; Get the image of an artwork (if it exists).
;; --------------------------------------------------------------------------------------------------
(defmethod wikidata-property ((artwork-data hash-table)
                              (property (eql :image))
                              &key &allow-other-keys)
  (let ((statement-id (get-statement-id "P18" artwork-data)))
    (if statement-id
      (let* ((statement-data (wikidata-get-statement statement-id :lisp-format :hash-table))
             (title (gethash "content" (gethash "value" statement-data)))
             (redirect-url (format nil 
                                   "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/~a"
                                   (cl-wikimedia::normalize-title title))))
        (values redirect-url statement-id))
      nil)))
; (wikidata-property *artwork-data* :image)

;; Get what is depicted (if exists)
;; --------------------------------------------------------------------------------------------------
(defmethod wikidata-property ((artwork-data hash-table)
                              (property (eql :depicts))
                              &key &allow-other-keys)
  (let ((artwork-id (gethash "id" artwork-data))
        (statement-id-list (listify (get-statement-id "P180" artwork-data))))
    (if statement-id-list
      (values (an-get-artwork-depicts :wikidata
                                      :id artwork-id)
              statement-id-list))))
;; (wikidata-property *artwork-data* :depicts)

;; Example for manual filter.
;; --------------------------
;; First some helper functions:
(defun wikidata-entity-p (uri)
  (ignore-errors (string= "http://www.wikidata.org/entity/"
                          (subseq uri 0 31))))
;; (wikidata-entity-p "http://www.wikidata.org/entity/Q5011")

(defun wikidata-date-p (string)
  (ignore-errors (string= "Z00:" (subseq (reverse string) 0 4))))
;; (wikidata-date-p "1530-01-01T00:00:00Z")

(defun my-filter-query-results (wd-id)
  (let* ((sparql-query (sparql-query-details wd-id))
         (http-response (wikidata-sparql-query sparql-query))
         (query-results (wikidata-query-results http-response)))
    (loop for query-result in query-results
          for rel = (extract-id-from-uri (gethash "value" (gethash "rel" query-result)))
          for rel-label = (gethash "value" (gethash "realpropertyLabel" query-result))
          for obj = (let ((obj-value (gethash "value" (gethash "obj" query-result))))
                      (cond ((wikidata-entity-p obj-value) (extract-id-from-uri obj-value))
                            ((wikidata-date-p obj-value) (parse-integer obj-value :junk-allowed t))
                            (t
                             obj-value)))
          for obj-label = (let ((obj-label-value (gethash "value" (gethash "objLabel" query-result))))
                            (if (wikidata-date-p obj-label-value)
                              (parse-integer obj-label-value :junk-allowed t)
                              obj-label-value))
          for result = (list rel rel-label obj obj-label)
          when (y-or-n-p (format nil "Do you like this value: ~a" result))
            collect result)))
#|
;; Example of manual filtering:
;; ----------------------------
(defparameter *my-filtered-results* nil)
(setf *my-filtered-results* (my-filter-query-results "Q686027"))
|#