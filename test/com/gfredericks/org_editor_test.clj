(ns com.gfredericks.org-editor-test
  (:require
   [clojure.spec.test.alpha         :as st]
   [clojure.string                  :as string]
   [clojure.test                    :refer [are deftest is testing use-fixtures]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators   :as gen]
   [clojure.test.check.properties   :as prop]
   [com.gfredericks.org-editor      :as org-editor])
  (:import
   (java.io StringReader)))

(use-fixtures :each
  (fn [t]
    (st/instrument (st/instrumentable-syms))
    (try
      (t)
      (finally
        (st/unstrument)))))

(deftest parse-file-test
  (are [in out] (= out (org-editor/parse-file (StringReader. in)))
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

(deftest read-tags-test
  (are [header tags] (= tags (org-editor/read-tags header))
    "* This has no tags"
    []

    "** TODO This has a couple tags              :welp:hokay:totes:"
    ["welp" "hokay" "totes"]))

(deftest set-tags-test
  (are [in-header tags out-header]
      (= out-header (org-editor/set-tags in-header tags))
    "*** TODO [#A] THOMAS THOMAS"
    []
    "*** TODO [#A] THOMAS THOMAS"

    "*** TODO [#A] THOMAS THOMAS"
    ["foo" "bar" "bengles"]
    "*** TODO [#A] THOMAS THOMAS                                 :foo:bar:bengles:"

    "*** TODO [#A] THOMAS THOMAS                                 :foo:bar:bengles:"
    ["weeeeeeelp"]
    "*** TODO [#A] THOMAS THOMAS                                      :weeeeeeelp:"))

(deftest conj-tag-test
  (are [in-header tag out-header]
      (= out-header (org-editor/conj-tag in-header tag))
    "* This is my header"
    "thomas"
    "* This is my header                                                  :thomas:"

    "* HEADED H HEDAD EH      UMMM    :jokes:jokes:"
    "could-be-anything"
    "* HEADED H HEDAD EH      UMMM                 :jokes:jokes:could-be-anything:"))

(deftest disj-tag-test
  (are [in-header tag out-header]
      (= out-header (org-editor/disj-tag in-header tag))
    "* This is my header"
    "thomas"
    "* This is my header"

    "* HEADED H HEDAD EH      UMMM    :jokes:foo:jokes:"
    "jokes"
    "* HEADED H HEDAD EH      UMMM                                           :foo:"))

(deftest add-categories-test
  (let [s
        "some random
preamble stuff
* Okay here's a header
* And here's another, with a category prop
  :PROPERTIES:
  :CATEGORY:    tompkins
  :END:

  more stuff maybe"
        parsed (-> s
                   StringReader.
                   org-editor/parse-file)]
    (testing "without a default"
      (let [[sec1 sec2] (-> parsed
                            (org-editor/add-categories)
                            (::org-editor/sections))]
        (is (contains? sec1 ::org-editor/category))
        (is (nil? (::org-editor/category sec1)))
        (is (= "tompkins" (::org-editor/category sec2)))))
    (testing "with a default"
      (let [[sec1 sec2] (-> parsed
                            (org-editor/add-categories "whatevs")
                            (::org-editor/sections))]
        (is (= "whatevs" (::org-editor/category sec1)))
        (is (= "tompkins" (::org-editor/category sec2)))))
    (testing "with a default and a global"
      (let [[sec1 sec2] (-> (str "some line\n#+CATEGORY: Goober Brigade\nother line\n" s)
                            StringReader.
                            org-editor/parse-file
                            (org-editor/add-categories "whatevs")
                            (::org-editor/sections))]
        (is (= "Goober Brigade" (::org-editor/category sec1)))
        (is (= "tompkins" (::org-editor/category sec2)))))))
