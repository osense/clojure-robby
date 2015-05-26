(defproject clojure-robby "1.1"
  :description "A simple genetic algorithm."
  :url "https://github.com/osense/clojure-robby"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot clojure-robby.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
