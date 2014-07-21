(defproject ninety-nine-clojure-problems "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [ninety-nine-clojure-problems.core]
  :main ninety-nine-clojure-problems.core)
