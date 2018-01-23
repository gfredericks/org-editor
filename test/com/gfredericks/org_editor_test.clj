(ns com.gfredericks.org-editor-test
  (:require
   [clojure.string                  :as string]
   [clojure.test                    :refer [are deftest is]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators   :as gen]
   [clojure.test.check.properties   :as prop]
   [com.gfredericks.org-editor      :as org-editor]))

(deftest parse-file-test
  (are [in out] (= out (org-editor/parse-file
                        (java.io.StringReader. in)))
    ""
    #::org-editor{:prelude  []
                  :sections []}

    "Welp!\n\n"
    #::org-editor{:prelude  ["Welp!" ""]
                  :sections []}

    (->> ["* First heading"
          "** TODO some todo thing"
          "   Look at my contents here"
          "* Another heading"
          "Last line"]
         (map #(str % \newline))
         (apply str))
    #::org-editor{:prelude  []
                  :sections [#::org-editor{:header   "* First heading"
                                           :prelude  []
                                           :sections [#::org-editor{:header "** TODO some todo thing"
                                                                    :prelude ["   Look at my contents here"]
                                                                    :sections []}]}
                             #::org-editor{:header   "* Another heading"
                                           :prelude  ["Last line"]
                                           :sections []}]}))

(def gen-org-line
  (gen/let [level (gen/frequency [[10 (gen/return 0)]
                                  [1  gen/nat]])
            line gen/string-ascii]
    (let [line (string/replace line #"[\n\r]" "")]
      (if (pos? level)
        (str (apply str (repeat level \*)) " " line)
        line))))

(def gen-org-file
  (gen/let [lines (gen/vector gen-org-line)]
    (->> lines
         (map #(str % \newline))
         (apply str))))

(defspec roundtrip-test 200
  (prop/for-all [s gen-org-file]
    (let [parsed (org-editor/parse-file (java.io.StringReader. s))
          unparsed (let [w (java.io.StringWriter.)]
                     (org-editor/write-file w parsed)
                     (str w))]
      (= s unparsed))))

(deftest read-properties-test
  (is (= {} (org-editor/read-properties
             #::org-editor{:header "* Thing"
                           :prelude ["whotaver" "just" "some" "lines"]
                           :sections []})))
  (is (= {"FOO" "tamb orine"
          "BAR" "12"}
         (org-editor/read-properties
          #::org-editor{:header "* Thing"
                        :prelude [":propeRTIES:"
                                  ":FOO: tamb orine  "
                                  "    :BAR:     12"
                                  "   :END:"
                                  "MORE"
                                  "    lines"]
                        :sections []}))))

(deftest prop-assoc-test
  (let [section #::org-editor{:header "*** This part here"
                              :prelude ["just" "some" "lines"]
                              :sections []}
        assoc'd (org-editor/prop-assoc section "K1" "K2")]
    (is (= assoc'd
           (assoc section ::org-editor/prelude
                  ["    :PROPERTIES:"
                   "    :K1: K2"
                   "    :END:"
                   "just"
                   "some"
                   "lines"])))
    (is (= assoc'd (org-editor/prop-assoc assoc'd "K1" "K2"))
        "Is idempotent")))
