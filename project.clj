(defproject org.dunaj/dunaj-lite "0.5.0"
  :description "Dunaj lite - An alternative core API for Clojure."
  :url "http://lite.dunaj.org"
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.dunaj/core.async "0.1.0-lite_pre5"]
                 [org.clojure/core.rrb-vector "0.0.11"]]
  :scm {:name "git" :url "https://github.com/dunaj-project/dunaj/tree/lite"}
  :signing {:gpg-key "6A72CBE2"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/jvm"]
  :main ^:skip-aot dunaj.main
  :aot [dunaj.core dunaj.main] ;; see CLJ-1650
;  :aot :all
;  :clean-non-project-classes true
  :auto-clean false
  :jar-exclusions [#"project\.clj"]
  :uberjar-exclusions [#"project\.clj"]
  :global-vars {*warn-on-reflection* true}
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :profiles {:uberjar {:aot [dunaj.core dunaj.main]
                       :omit-source true}}
;;  :jvm-opts ^:replace ["-Xms3G" "-Xmx3G" "-XX:-UseConcMarkSweepGC" "-server"]
)
