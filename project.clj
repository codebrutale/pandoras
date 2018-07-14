(defproject codebrutale/pandoras "0.1.0-SNAPSHOT"
  :description "A mixed bag of Clojure stuff"
  :url "https://github.com/codebrutale/pandoras"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["snapshots" :clojars]
                        ["releases" :clojars]]
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :source-paths ["src/lib"]
  :test-paths ["src/test"]
  :profiles {:dev {:source-paths ["src/dev" "src/test"]}
             :test {}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}}
  :aliases {"with-test-profiles" ["with-profiles" "test:test,1.8:test,1.7"]
            "test-all" ["with-test-profiles" "test"]})
