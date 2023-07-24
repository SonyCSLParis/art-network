(in-package :asdf)

(defsystem :art-network
  :description "Art Explorer"
  :author "Sofia Baroncini, Luc Steels and Remi van Trijp"
  :version "0.1"
  :license "Apache 2.0"
  :depends-on (:utils :cl-store :fcg-server :monitors :irl :fcg :nlp-tools :cl-wikimedia :web-interface :propbank-grammar :test-framework
               :s-base64 :xmls :inn)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:module "irl"
                :serial t
                :components ((:file "entities")
                             (:file "primitives")))
               (:module "discourse-model"
                :serial t
                :components ((:file "class")))
               (:module "INN" ;; Integrative Narrative Networks
                :serial t
                :components ((:file "classes")
                             (:file "events")
                             (:file "initial-network")))
               (:module "expert-system"
                :serial t
                :components ((:file "expert-narrative-questions")))
               (:module "ontologies"
                :serial t
                :components ((:file "config")
                             (:file "sparql")
                             (:file "knowledge-base")
                             (:file "icon-ontology")))
               (:module "knowledge-graph"
                :serial t
                :components ((:file "api")
                             (:file "sparql")
                             (:file "wikidata")))
               (:module "graph-functions"
                :serial t
                :components ((:file "simplify-graph-creation")))
               (:file "explorer")
               (:module "interface"
                :serial t
                :components ((:file "registry")
                             (:file "start-screen")
                             (:file "forms")
                             (:file "ajax-javascript")))))