; KG/graph/sem-networks.lsp
;; first load: KG/code/build-network

;;(load "build-trana-net.lisp")
(ql:quickload :art-network)
(in-package :irl)

(progn 

(init-net 50) ; starting up files to store nodes and edges arg = maximum nr of time steps expected

;;; introducing the painting 

(def-step introducing-painting-secrets) ; 1 

(defc painting-secrets 2 () ; from catalog 
      (image image-painting-secrets)
      (is-a painting)
      (genre portrait)
      (medium oil-on-canvas)
      (owner private-collection)
      (year year-1990)
      (in-exhibition exhibition-la-pelle)
      (courtesy Zeno-X-Gallery))

(defc exhibition-la-pelle 2 ())

;;;; Introducing the painter 

(def-step tuymans)  ; 2 

(defc painting-secrets 2 () ; from catalog 
      (artist luc-tuymans)
      (in-exhibition exhibition-la-pelle)
      (courtesy Zeno-X-Gallery))

(defc luc-tuymans 3 ()
      (is-a painter)
      (photo photo-luc-tuymans)
      (nationality belgian)
      (living-in Antwerp)
      (gallery Zeno-X-Gallery)
      (gallery David-Zwirner)
      (born year-1958)
      (exhibition exhibition-La-Pelle))

(defc photo-luc-tuymans 1 ()
      (image image-luc-tuymans))

;;; Introducing the title 

(def-step introducing-title-secrets) ; 3 

(defc painting-secrets 2 () 
       (title secrets))

(defc secrets 3 ()
      (is information)
      (stored-in hidden-location)
      (known-by few-people)
      (kept-away-from other-people) 
      (distribution forbidden)
      (access restricted)
      (content secret-content)
      (keeper secret-keeper)
      (synonym mystery)
      (motivation motivation-for-secrets)) ; cambridge dictionary/ macmillan dictiony

(defc motivation-for-secrets 1 ()
      (could-be crime)
      (could-be manipulation))

;;; First interpretation: it is a face (= portrait)
;;; Find focal region 

(def-step focal-region) ; 4 

(defc painting-secrets 2 ()
      (depicted face-secrets)
      (MSI-algorithm visual-focal-point))

(defc visual-focal-point 1 ()
      (image image-focal-point))

;;; inward looking 

(def-step face-interpretation) ; 5 

(defc face-secrets 1 ()
      (expression closed-off-face)
      ) ; from catalog 

(defc looking-inward 1 ()
      (is-a facial-expression)
      (mental-state self-reflection)
      (suggests hiding-secrets)
      (grounding visual-focal-point))

;;; closed-off

(def-step closed-off) ; 6 

(defc face-secrets 1 ()
      (expression closed-off-face))

(defc closed-off-face 1 ()  ; dictionary
      (is-a facial-expression)
      (social-attitude avoid-emotional-connection)
      (mental-state redrawn) 
      (suggests hiding-secrets)
      (grounding visual-focal-point))

;;;;;;;;;;

(def-step color-analysis) ; 7 

(defc painting-secrets 2 ()
      (k-means-clustering cluster-5-palette))

(defc secrets-clustered 3 ()
      (image image-k-means-cluster-5))

(defc cluster-5-palette 1 ()
      (image image-cluster-5-palette)
      (clustered-image secrets-clustered))

(defc tight-lipped-color 1 ()
      (image image-tight-lipped)
      (pattern-from secrets-clustered))

;;;;;; what does tight-lipped mean? 

(def-step tight-lipped) ; 8 

(defc face-secrets 1 ()
      (facial-expressions facial-expressions-speer))

(defc facial-expressions-speer 3 ()
      (facial-expression tight-lipped))

(defc tight-lipped 2 ()
      (is-a facial-expression)
      (grounding tight-lipped-color)
      (lip-position lips-pressed-together)
      (suggests determined-reticence)
      (suggests reluctance-to-speak)
      (suggests hiding-secrets))  ;; https://dictionary.cambridge.org/dictionary/english/tight-lipped

(defc reluctance-to-speak 1 ()
      (is-a social-attitude))

(defc determined-reticence 1 ()
      (is-a social-attitude))

(defc hiding-secrets 1 ()
      (is-a social-attitude)
      (hiding secrets)
      )

;;;;;; focal regions spectral residual

(def-step salient-region-spectral-residual) ; 9

(defc painting-secrets 1 ()
      (spectral-residual secrets-spectral-residual))

(defc secrets-saliency-spectral 1 ()
      (image image-spectral-residual))

(defc tight-lipped 2 ()
      (grounding tight-lipped-focal))

(defc tight-lipped-focal 1 ()
      (image image-tight-lipped-2)
      (pattern-from secrets-saliency-spectral))

;; ;; more interpretations


(def-step tight-lipped-more) ; 10 

(defc tight-lipped 2 ()
      (suggests avoid-showing-anger))

(defc avoid-showing-anger 1 ()
      (is-a suppressed-emotion)
      (emotion anger))

;;;;

(def-step TIN-edge-detection) ; 11

(defc painting-secrets 3 ()
      (TIN-edges secrets-TIN-edges))

(defc secrets-TIN-edges 2 ()
      (image image-TIN-edges))

(defc noticed-sharp-nose 1 ()
      (image image-hawk-nose)
      (pattern-from secrets-TIN-edges))

;; interpretation 

(def-step sharp-nose) ; 12

(defc sharp-nose 2 ()
      (belongs-to face-secrets)
      (body-part nose)
      (feature sharp)
      (suggests authoritarian)
      (suggests arrogance)
      (grounding noticed-sharp-nose))

;;;;;

(def-step secrets-closed-eyelids) ; 13 

(defc noticed-closed-eyelids 1 ()
      (image image-closed-eyes)
      (pattern-from secrets-clustered))


(defc noticed-closed-eyelids-saliency 1 ()
      (image image-closed-eyes-2)
      (pattern-from secrets-saliency-spectral))

(defc closed-eyelids 2 ()
      (grounding noticed-closed-eyelids)
      (grounding noticed-closed-eyelids-saliency))

;;; interpretation of closed-eyelids

(def-step interpret-closed-eyelids) ; 14 

(defc facial-expressions-speer 3 ()
      (facial-expression closed-eyelids))

(defc closed-eyelids 2 ()
      (eyelids closed)
      (suggests denial-of-reality)
      (suggests self-reflection)
      (suggests hiding-secrets)
      (suggests dishonest)
      (suggests death))

(defc denial-of-reality 1 ()
      (is-an attitude))

(defc self-reflection 1 ()
      (is-an attitude))

;;;;; mustache 

(def-step mustache) ; 15

(defc face-secrets 1 ()
      (facial-features facial-features-speer))

(defc facial-features-speer 2 ()
      (facial-feature mustache))

(defc mustache 1 ()
      (is-a facial-feature)
      (grounding noticed-mustache)
      (body-part area-under-nose)
      (feature hairy)
      (suggests masculinity)
      (suggests military)
      (suggests authoritarian))

(defc noticed-mustache 1 ()
      (image image-speer-mustache)
      (pattern-from secrets-clustered))
;;;;;

(def-step eye-brow) ; 16

(defc facial-features-speer 3 ()
      (facial-feature s-shaped-eye-brow)
      (facial-feature thick-eye-brows))

(defc s-shaped-eye-brow 2 ()
      (is-a facial-feature)
      (body-part eye-brows)
      (feature s-shaped)
      (grounding noticed-eye-brow)
      (associated-with aggression)
      )

(defc thick-eye-brows 2 ()
      (is-a facial-feature)
      (body-part eye-brows)
      (feature thick)
      (grounding noticed-eye-brow)
      (associated-with narcissism)
      (associated-with possessive))

(defc noticed-eye-brow 1 ()
      (image image-eye-brow)
      (pattern-from secrets-clustered))

(defc aggression 1 ()
      (is-an emotion)
      (suggests terror)
      (suggests causing-harm)
      (suggests threatening))

(defc narcissism 1 ()
      (is-an attitude)
      (suggests arrogance)
      (suggests self-admiration))

;;;;;;;;;;;;;

(def-step open-nostrils) ; 17

(defc facial-features-speer 3 ()
      (facial-feature open-nostrils))

(defc open-nostrils 1 ()
      (is-a facial-feature)
      (body-part nostrils)
      (feature open)
      (grounding noticed-open-nostrils)
      (suggests anger)
      (suggests aggression)
      (suggests authoritarian))

(defc noticed-open-nostrils 1 ()
      (image image-open-nostrils)
      (pattern-from secrets-clustered))

;;; hollow eyes 

(def-step sunken-eyes) ; 18 

(defc facial-features-speer 3 ()
      (facial-feature sunken-eyes))

(defc sunken-eyes 2 ()
      (is-a facial-feature)
      (body-part eye-sockets)
      (position hollow)
      (suggests death)
      (suggests fatigue) 
      (grounding noticed-sunken-eyes-color)
      (grounding noticed-sunken-eyes-edges))

(defc noticed-sunken-eyes-color 1 ()
      (image image-closed-eyes)
      (pattern-from secrets-clustered))
 
(defc noticed-sunken-eyes-edges 1 ()
      (image image-closed-eyes-2)
      (pattern-from secrets-TIN-edges))

;;;;

(def-step rectangular-face-shape) ; 19

(defc facial-features-speer 3 ()
      (facial-feature rectangular-face-shape))

(defc rectangular-face-shape 2 ()
      (is-a facial-feature)
      (face-shape rectangular)
      (suggests death)
      (suggests authoritarian)
      (suggests planner)
      (grounding noticed-rectangular-face))

(defc noticed-rectangular-face 1 ()
      (image image-rectangular-face)
      (pattern-from secrets-TIN-edges))

;;;

(def-step identify-speer) ; 20 

(defc painting-secrets 2 ()
      (depicted face-secrets))

(defc face-secrets 1 ()
      (face-of albert-speer)
      (is secret-keeper))

(defc albert-speer 2 ()
      (member-of Nazi-party)
      (minister Third-Reich)
      (role minister-of-armament) ; from catalog
      (politics fascism)
      (politics nationalism)
      (book Spandau-the-secret-diaries)
      (role minister-of-armament)
      (profession architect))

(defc spandau-the-secret-diaries 1 ()
      (is-a book)
      (author Albert-Speer)
      (topic secrets)
      (topic nazi-party)
      (topic fascism)
      (topic imprisonment)
      (context world-war-II)
      (theme moral-decay))

;;; political

(def-step political) ;; 21

(defc fascism 1 ()
      (is-a political-movement)
      (spectrum far-right)
      (identity ultra-nationalism)
      (government-form totalitarian)
      (social-attitude authoritarian))

(defc Nazi-regime 1 ()
      (controlled-by nazi-party)
      (flag picture-nazi-flag)
      (country germany)
      (politics fascism)
      (politics nationalism)
      (associated-with terror)
      (leader adolf-hitler))

(defc picture-nazi-flag 1 ()
      (image image-nazi-flag))

(defc adolf-hitler 1 ()
      (is-a politician)
      (leader-of nazi-regime)
      (image image-adolf-hitler)
      (character authoritarian)
      (politics fascism)
      (politics nationalism)
      (associated-with terror)
      (member-of nazi-party))

;;;;

(def-step picture-speer) ;; 22 

(defc albert-speer 2 ()  ; from google image search 
      (picture face-albert-speer)
      (picture photo-speer-nazi)
      (picture photo-head-albert-speer))

(defc face-albert-speer 1 () 
      (image image-albert-speer)
      (section-of photo-speer-nazi))

(defc photo-speer-nazi 1 ()
      (image image-speer-nazi))

(defc photo-head-albert-speer 1 ()
      (image image-speer-nazi-head))

;;;; mapping

(def-step mapping-face-speer)  ; 23 

(defc painting-secrets 2 () ;; 23 
      (source face-albert-speer))

(defc face-albert-speer 1 ()
      (mapping-algorithm mapping-source-target))

(defc mapping-source-target 1 ()
      (image image-mapping-source-target))

(defc painting-secrets 2 ()
      (mapping-algorithm mapping-source-target))

;;;;;;;;;;

(def-step exhibition-la-pelle) ;; 24

(defc exhibition-la-pelle 2 ()
  (is-an exhibition)
  (title la-pelle)
  (artist luc-tuymans)
  (location palazzo-grassi)
  (begin march-2019)
  (end jan-2020)
  (curator caroline-bourgeois))
      
;      (face-form geometrical-face)
;      (face-character flattened-skin)
;      (face-character s-shaped-eye-brow)
;      (face-character mustache))

(defc palazzo-grassi 2 ()
   (is-a building)
   (photo photo-palazzo-grassi)
   (city venice)
   (location gran-canale)
   (address Campo-San-Samuele-3231)
   (function museum)
   (owner Francois-Pinault))

(defc photo-palazzo-grassi 2 ()
      (image image-palazzo-grassi))

;; film la pelle

(def-step film-la-pelle) ;; 25 

(defc film-la-pelle 1 ()
   (is-a film)
   (picture picture-film-la-pelle)
   (title la-pelle)
   (director liliana-cavalli)
   (inspiration book-la-pelle)
   (actor Marcello-Mastroianni)
   (actress Claudia-Cardinale)
   (actor Burt-Lancaster)
   (context world-war-II)
   (context American-liberation)
   (release-date year-1981)
   (theme moral-decay)
   (location Naples))

(defc picture-film-la-pelle 1 ()
      (image image-film-la-pelle))

;;; La Pelle - book 

(def-step book-la-pelle)  ;; 26

(defc book-la-pelle 1 ()
   (is-a book)
   (title la-pelle)
   (picture picture-book-la-pelle)
   (author curzio-malaparte)
   (period year-1945)
   (context world-war-II)
   (context American-liberation)
   (location Naples)
   (location Italy)
   (theme moral-decay)
   (published year-1949))

(defc picture-book-la-pelle 1 ()
      (image image-book-la-pelle))

;;; Curzio Malaparte 

(defc curzio-malaparte 1 ()
   (is-a person)
   (picture picture-curzio-malaparte)
   (nationality italian)
   (location Capri)
   (born year-1898)
   (author-of book-la-pelle)
   (real-name Curt-Suckert)
   (home villa-malaparte)
   (politics fascism)
   (politics communism))

(defc picture-curzio-malaparte 1 ()
      (image image-curzio-malaparte))

;;; La pelle as skin 

(def-step body-la-pelle) ;; 27

(defc skin-la-pelle 1 ()
   (is-a body-part)
   (body-part skin)
   (italian la-pelle))

;;;;;;;;;

; painting le mepris

(def-step villa-malaparte) ;; 28 

(defc villa-malaparte 1 ()
   (is-a house)
   (picture picture-villa-malaparte)
   (location Capri)
   (owner curzio-malaparte)
   (film-set-for film-le-mepris))

(defc picture-villa-malaparte 1 ()
      (image image-villa-malaparte))

(defc painting-le-mepris 1 ()
      (is-a painting)
      (picture picture-painting-le-mepris)
      (depicted fireplace)
      (title le-mepris)
      (artist luc-tuymans)
      (exhibition exhibition-la-pelle)
      (year year-2015)
      (source fireplace-villa-malaparte)
      (source film-le-mepris)
      (theme contempt)
      (theme deception)
      (dimensions 112-142cm))

(defc picture-painting-le-mepris 1 ()
      (image image-painting-le-mepris))

(defc fireplace-villa-malaparte 1 () 
      (is-a fireplace)
      (location villa-malaparte)
      (image image-fireplace))

(def-step film-le-mepris) ;; 29

(defc film-le-mepris ()
   (is-a film)
   (title le-mepris)
   (image image-film-le-mepris)
   (director jean-luc-godard)
   (actress Brigitte-Bardot)
   (actor Michel-Piccoli)
   (location villa-malaparte)
   (date year-1963))

(close-net)
)

