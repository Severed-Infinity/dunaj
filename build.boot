;; Copyright (C) 2015, Jozef Wagner. All rights reserved.

;;; Configuration

(def version "0.7.0-SNAPSHOT")

;;; Boot scripts

(require 'clojure.edn)

(def lite? (get (System/getenv) "DUNAJ_LITE"))

(if lite?
  (println "mode: dunaj-lite")
  (println "mode: dunaj"))

(when lite?
  (set-env! :target-path "target-lite"))

(set-env!
  :resource-paths #{"src/clj"}
  :source-paths #{"src/jvm"}
  :dependencies (if lite?
                  '[[org.clojure/clojure "1.7.0"]
                    [org.dunaj/core.async "0.1.0-lite_pre5"]
                    [org.clojure/core.rrb-vector "0.0.11"]]
                  '[[org.dunaj/clojure "1.7.0-dunaj_R1"]
                    [org.dunaj/core.async "0.1.0-dunaj_pre4"]
                    [org.dunaj/core.rrb-vector "0.0.12-dunaj_pre4"]])
  :exclusions (if lite?
                []
                '[org.clojure/clojure org.clojure/clojurescript]))

(task-options!
 pom (if lite?
       {:project 'org.dunaj/dunaj-lite
        :version version
        :description "Dunaj lite - An alternative core API for Clojure."
        :url "http://lite.dunaj.org"
        :scm {:url "https://github.com/dunaj-project/dunaj"}
        :license {"Eclipse Public License - v 1.0" "http://www.eclipse.org/legal/epl-v10.html"}}
       {:project 'org.dunaj/dunaj
        :version version
        :description "Dunaj - An alternative core API for Clojure."
        :url "http://www.dunaj.org"
        :scm {:url "https://github.com/dunaj-project/dunaj"}
        :license {"Eclipse Public License - v 1.0" "http://www.eclipse.org/legal/epl-v10.html"}})
 aot {:namespace '[bare.core dunaj.core dunaj.main]}
 push {:gpg-sign true
       :gpg-user-id "Jozef Wagner (Dunaj Project) <wagjo@wagjo.com>"
       :gpg-keyring "/home/wagjo/.gnupg/secring.gpg"
       :repo "clojars-upload"})

(ns-unmap *ns* 'repl)
(ns-unmap *ns* 'install)

(deftask repl
  "Dunaj repl"
  []
  (comp (javac) (boot.task.built-in/repl)))

(deftask build
  "Dunaj build"
  []
  (comp (pom) (aot) (jar)))

(deftask install
  "Dunaj install"
  []
  (comp (build) (boot.task.built-in/install)))

(deftask clojars
  "Dunaj clojars"
  []
  (print "Enter Clojars password: ")
  (set-env!
   :repositories
   #(conj % ["clojars-upload"
             {:url "https://clojars.org/repo"
              :username "wagjo"
              :password (apply str (.readPassword (System/console)))}]))
  (comp (build) (push)))
