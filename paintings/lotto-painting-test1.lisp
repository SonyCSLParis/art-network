
(ql:quickload :art-network)
;; (in-package :art-network)

;; (ql:quickload :web-interface)
;; (load "systems/web-interface/vis-js.lisp")

(defun format-node (id label)
  (format nil "{id: '~a', label: '~a'" id label}))



(wi::activate-vis-js)


(wi::add-element `((div :id "demoNetwork")
               ,(wi::make-vis-network :element-id "demoNetwork"
                                  :nodes '("{id: 'node1', label: 'Node 1'}"
                                           "{id: 'node2', label: 'Node 2'}"
                                           "{id: 'node3', label: 'Node 3'}"
                                           "{id: 'node4', label: 'Node 4'}"
                                           "{id: 'node5', label: 'Node 5'}")
                                  :edges '("{from: 'node1', to: 'node3'}"
                                           "{from: 'node1', to: 'node2'}"
                                           "{from: 'node2', to: 'node4'}"
                                           "{from: 'node2', to: 'node5'}"))))

(wi::vis-destroy-network)


(wi::add-element `((div :id "LottoNetwork1")
               ,(wi::make-vis-network :element-id "LottoNetwork1"
                                  :nodes '("{id:'OBJ1002test', label:'Lorenzo Lotto, Venus and Cupid', shape: 'circularImage', image:'https://images.metmuseum.org/CRDImages/ep/original/DP-25975-001.jpg'}"
                                           "{id: 'OBJ1002test-title', label: 'Venus and Cupid'}"
                                           "{id: 'date-1520-1530', label: '1520s'}"
                                           "{id: 'lorenzo-lotto', label: 'Lorenzo Lotto'}"
                                           "{id: 'met-museum', label: 'MET Museum'}")
                                  :edges '("{from: 'OBJ1002test', to: 'OBJ1002test-title', label:'has title'}"
                                           "{from: 'OBJ1002test', to: 'date-1520-1530', label:'has date'}"
                                           "{from: 'OBJ1002test', to: 'lorenzo-lotto', label:'has author'}"
                                           "{from: 'OBJ1002test', to: 'met-museum', label:'located in'}"))))

(wi::add-element `((div :id "LottoNetwork1")
               ,(wi::make-vis-network :element-id "LottoNetwork2"
                                  :nodes '("{id:'OBJ1002test', label:'Lorenzo Lotto, Venus and Cupid', shape: 'circularImage', image:'https://images.metmuseum.org/CRDImages/ep/original/DP-25975-001.jpg'}"
                                           "{id: 'OBJ1002test-title2', label: 'title2'}"
                                           "{id: 'date-1520-1530', label: '1520s'}"
                                           "{id: 'lorenzo-lotto', label: 'Lorenzo Lotto'}"
                                           "{id: 'met-museum', label: 'MET Museum'}")
                                  :edges '("{from: 'OBJ1002test', to: 'title2', label:'has new title'}"
                                           "{from: 'OBJ1002test', to: 'date-1520-1530', label:'has date'}"
                                           "{from: 'OBJ1002test', to: 'lorenzo-lotto', label:'has author'}"
                                           "{from: 'OBJ1002test', to: 'met-museum', label:'located in'}"))))




(let ((murray (list "Bob" "https://fr.web.img2.acsta.net/newsv7/20/12/07/18/08/0772723.png"))
      (johansson (list "Charlotte" 
                       "https://focus.telerama.fr/967x550/100/2021/03/29/7b620e16-079e-4f47-a085-e09b0a897ff9.jpg"))
      (ribisi (list "John" 
                    "https://i0.wp.com/nobadmovie.com/wp-content/uploads/2021/07/giovanniribisi_lostintranslation_box.png"))
      (faris (list "Kelly"
                   "https://m.media-amazon.com/images/M/MV5BZWUzZTM3MzEtMmQwZi00ZWExLThhNzYtMGQwODFlMTk2NzNhXkEyXkFqcGdeQXVyNDA4MDkxNzE@._V1_.jpg")))
  (add-element 
   `((div :id "LostInTranslation")
     ,(make-vis-network :element-id "LostInTranslation"
                        :nodes (loop for character in (list murray johansson ribisi faris)
                                     collect (format nil "{id: '~a', label: '~a', shape: 'circularImage', image: '~a'}"
                                                     (first character) (first character) (second character)))
                        :edges '("{from: 'Charlotte', to: 'Bob', label: 'friends'}"
                                 "{from: 'Charlotte', to: 'John', label: 'married'}"
                                 "{from: 'John', to: 'Kelly', label: 'co-workers'}")))))



;; idea: create a function to add nodes and edges directly from a csv file
;; the file should be like a list of lists (= the rows) 
;; maybe following this: https://monkeyjunglejuice.github.io/blog/learning-to-code-first-app-episode-1.tutorial.html 
;; example made by hand. we should find a way to open a csv and read it as a list of lists

(defvar my_file '(("wdt:P19"	"place of birth"	 "Q641"	"Venice")
                  ("wdt:P569"	"date of birth"	"1480" "1 January 1480")
                  ("wdt:P570"	"date of death"	"1556" "1 January 1556")
                  ("wdt:P135"	"movement"	 "Q131808"	"mannerism")
                  ("wdt:P135"	"movement"	 "Q1474884"	"High Renaissance")
                  ("wdt:P937"	"work location"	 "Q641"	"Venice")
                  ("wdt:P937"	"work location" "Q220"	"Rome")
                  ("wdt:P937"	"work location" "Q3415"	"Ancona")
                  ("wdt:P937"	"work location" "Q5475"	"Treviso")))




;; try to add each node and edge
(vis-add-node "{id: 'node7', label: 'node 7'}")
(vis-add-edge "{from: 'node1', to: 'node7'}")

(loop for line in my_file do (vis-add-node (format nil "{id: '~a', label: '~a'}"
                                                     (third line) (fourth line))))
(loop for line in my_file do (vis-add-edge (format nil "{from: 'lorenzo-lotto', to: '~a', label: '~a'}"
                                                      (third line)(second line))))



;; problem: if you do that, you create a new graph and you do not update the previous one
(add-element 
   `((div :id "LottoDetails")
     ,(wi::make-vis-network :element-id "LottoDetails"
                        :nodes (loop for line in my_file
                                     collect (format nil "{id: '~a', label: '~a'}"
                                                     (third line) (fourth line)))
                        :edges (loop for line in my_file
                                     collect (format nil "{from: 'lorenzo-lotto', to: '~a', label: '~a'}"
                                                      (third line)(second line))))))




