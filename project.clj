(defproject  com.gfredericks/org-editor "0.1.3"
  :description "Some clojure code for reading and writing org files"
  :url "https://github.com/gfredericks/org-editor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :plugins [[com.gfredericks/how-to-ns "0.1.8"]]
  :how-to-ns {:require-docstring? false
              :align-clauses? true}
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.9.0"]]}})
