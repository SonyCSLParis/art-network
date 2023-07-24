
; (ql:quickload :art-network)
(ql:quickload "cl-csv")
(ql:quickload "cl-json")
(in-package :art-network)

;; Test the interface so far:
(progn
  (setf cl-wikimedia::*user-agent* "firstname.surname@gmail.com")
  (launch-artwork-explorer :open-web-interface t))

;; Example: what is depicted in an artwork according to Wikidata?
(an-get-artwork-depicts :wikidata :id "Q4009580")
;; Only labels:
(mapcar #'first (an-get-artwork-depicts :wikidata :id "Q4009580"))


;; test for the the manual filtering:
(defparameter *my-filtered-results* nil)
(setf *my-filtered-results* (my-filter-query-results "Q686027"))
;; results:  (("P31" "instance of" "Q12119802" "lyric poetry genre") ("P279" "subclass of" "Q5185279" "poem")) 

(defun stringify-list-of-labels (my-labels)
  (format nil "~{~a~^,~}" my-labels))
;; (stringify-list-of-labels '("label1" "label2" "label3")) 
;; => "label1|label2|label3"
