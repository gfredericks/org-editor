(ns com.gfredericks.org-editor
  (:require
   [clojure.java.io    :as io]
   [clojure.spec.alpha :as s]
   [clojure.string     :as string]
   [clojure.walk       :as walk]))

(s/def ::file (s/keys :req [::prelude ::sections]))
(s/def ::line (s/and string? #(not (re-find #"[\n\r]" %))))
(s/def ::lines (s/coll-of ::line :kind sequential?))
(s/def ::prelude ::lines)
(s/def ::sections (s/coll-of ::section :kind sequential?))
(s/def ::section (s/keys :req [::header ::prelude ::sections]))
(def header-line-regex #"(\*+) (.*)")
(s/def ::header (s/and string? #(re-matches header-line-regex %)))

(defn ^:private timestamp-line?
  [line]
  ;; obvs this could be made more precise
  (re-find #"DEADLINE|SCHEDULED" line))

(s/fdef parse-file
        :args (s/cat :reader #(instance? java.io.Reader %))
        :ret ::file)
(defn parse-file
  [reader]
  (with-open [br (java.io.BufferedReader. reader)]
    (let [lines (line-seq br)

          [prelude sections]
          ((fn parse-prelude-and-sections [numbered-lines min-level]
             (let [[numbered-prelude more-numbered-lines]
                   (split-with #(not (re-matches header-line-regex (first %)))
                               numbered-lines)

                   prelude (map first numbered-prelude)]
               (loop [sections       []
                      numbered-lines more-numbered-lines]
                 (if (empty? numbered-lines)
                   [prelude sections []]
                   (let [[[line line-number] & more-numbered-lines] numbered-lines
                         [whole-line level] (re-matches header-line-regex line)]
                     (if (< (count level) min-level)
                       [prelude sections numbered-lines]
                       (let [[prelude subsections more-numbered-lines]
                             (parse-prelude-and-sections more-numbered-lines (inc (count level)))]
                         (recur (conj sections
                                      (vary-meta
                                       #::{:header   whole-line
                                           :prelude  prelude
                                           :sections subsections}
                                       assoc ::line-number line-number))
                                more-numbered-lines))))))))
           (map vector lines (rest (range))) 1)]
      #::{:prelude  prelude
          :sections sections})))

(defn write-file
  [writer file]
  (with-open [pw (java.io.PrintWriter. writer)]
    (binding [*out* pw]
      ((fn func [{::keys [prelude sections]}]
         (run! println prelude)
         (doseq [section sections]
           (println (::header section))
           (func section)))
       file))))

(defn indent-lines
  [level lines]
  (let [space (apply str (repeat (inc level) \space))]
    (map #(str space %) lines)))

(defn read-level
  [section]
  (->> (::header section)
       (take-while #{\*})
       (count)))

;; TODO: org-mode seems to allow the properties section to appear anywhere
;; in the prelude, despite the documentation, and in any case it intentionally
;; inserts other things above properties, like logbook entries and other notes
(s/def ::prelude-with-properties
  (s/cat :scheduling-line   (s/? ::line)
         :prop-start        #(re-matches #"(?i)\s*:PROPERTIES:\s*" %)
         :props             (s/* #(not (re-matches #"(?i)\s*:END:\s*" %)))
         :prop-end          #(re-matches #"(?i)\s*:END:\s*" %)
         :remaining-prelude (s/* ::line)))

(def prop-line-pattern #"(\s*):([^:\s]+):(\s+)(.*?)\s*")

(defn read-properties
  [section]
  (let [conformed (s/conform ::prelude-with-properties (::prelude section))]
    (if (= ::s/invalid conformed)
      {}
      (->> (:props conformed)
           (keep (fn [line]
                   (when-let [[_ _ k _ v] (re-matches prop-line-pattern line)]
                     [k v])))
           (into {})))))

(defn prop-get
  "Looks up the given property value, in a case-insensitive manner."
  [section k]
  (let [k (string/lower-case k)]
    (->> (read-properties section)
         (some (fn [[k' v]]
                 (and (= k (string/lower-case k')) v))))))

(defn prop-assoc
  ([section k v]
   (let [{::keys [prelude]} section
         conformed (s/conform ::prelude-with-properties prelude)]
     (if (= ::s/invalid conformed)
       (let [prop-lines (indent-lines (read-level section)
                                      [":PROPERTIES:"
                                       (format ":%s: %s" k v)
                                       ":END:"])]
         (assoc section ::prelude
                (if (timestamp-line? (first prelude))
                  (concat (take 1 prelude) prop-lines (drop 1 prelude))
                  (concat prop-lines prelude))))

       (let [kv-pair-lines
             (loop [passed-props []
                    more-props (:props conformed)]
               (if (empty? more-props)
                 ;; no matches, add a new line
                 (concat passed-props
                         (indent-lines (read-level section)
                                       [(format ":%s: %s" k v)]))
                 (or (if-let [[_ whitespace1 k' whitespace2 v']
                              (re-matches prop-line-pattern (first more-props))]
                       (if (= (string/lower-case k)
                              (string/lower-case k'))
                         ;; found a match; update this line
                         (concat passed-props
                                 [(str whitespace1 ":" k' ":" whitespace2 v)]
                                 (rest more-props))))
                     (recur (conj passed-props (first more-props))
                            (rest more-props)))
                 ))]
         (assoc section ::prelude
                (concat (some-> (:scheduling-line conformed) vector)
                        [(:prop-start conformed)]
                        kv-pair-lines
                        [(:prop-end conformed)]
                        (:remaining-prelude conformed)))))))
  ([section k v & more-kvs]
   (reduce (fn [section [k v]] (prop-assoc section k v))
           section
           (cons [k v] (partition 2 more-kvs)))))

(defn prelude-tail
  [{::keys [prelude]}]
  "Returns the prelude of the given section, without a scheduling line or
  properties drawer."
  (let [conformed (s/conform ::prelude-with-properties prelude)]
    (if (= ::s/invalid conformed)
      (if (and (seq prelude) (timestamp-line? (first prelude)))
        (rest prelude)
        prelude)
      (:remaining-prelude conformed))))

;;
;; Tags
;;

(def ^:private header-with-tags-regex
  #"(.*?)\s*(:(?:[^\s:]+:)+\s*)?$")

(s/def ::tag (s/and string?
                    #(re-matches #"^[^:\s]+$" %)))
(s/def ::tags (s/coll-of ::tag :kind sequential?))

(s/fdef read-tags
        :args (s/cat :header ::header)
        :ret ::tags)
(defn read-tags
  "Returns a sequence of strings"
  [header]
  (let [[_ pre-tags tags] (re-matches header-with-tags-regex header)]
    (if tags
      (rest (string/split tags #":"))
      ())))

(s/fdef set-tags
  :args (s/cat :header ::header :tags ::tags)
  :ret ::header)
(defn set-tags
  [header tags]
  (let [[_ pre-tags] (re-matches header-with-tags-regex header)]
    (if (empty? tags)
      pre-tags
      (let [tags-section (format ":%s:" (string/join ":" tags))]
        (str pre-tags
             ;; org-mode aims these to make the line 77 chars wide
             (apply str (repeat
                         (-> 77
                             (- (count pre-tags) (count tags-section))
                             (max 1))
                         \space))
             tags-section)))))

(s/fdef conj-tag
  :args (s/cat :header ::header :tag ::tag)
  :ret ::header)
(defn conj-tag
  "Adds the given tag to the end of the tags section, if the tag
  isn't already there."
  [header tag]
  (let [tags (read-tags header)]
    (if (some #{tag} tags)
      header
      (set-tags header (concat tags [tag])))))

(s/fdef disj-tag
  :args (s/cat :header ::header :tag ::tag)
  :ret ::header)
(defn disj-tag
  "Removes the tag from the header, including duplicates."
  [header tag]
  (set-tags header (remove #{tag} (read-tags header))))

(s/fdef add-categories
        :args (s/cat :file ::file
                     :default (s/? (s/nilable string?)))
        :ret ::file
        ;; could have a :fn that asserts that every section
        ;; has a category key and that the input and output
        ;; are otherwise identical
        )
(defn add-categories
  "Decorates all sections in the file
  with :com.gfredericks.org-editor/category keys, aiming to use the
  same logic that org-mode uses."
  ([file] (add-categories file nil))
  ([file default]
   (let [global (->> (::prelude file)
                     (some #(re-matches #"#\+CATEGORY:\s+(.*?)\s*" %))
                     (second))
         default (or global default)
         section-fn (fn section-fn [section default]
                      (let [cat-this-section (prop-get section "CATEGORY")
                            cat (or cat-this-section default)]
                        (-> section
                            (assoc ::category cat)
                            (update ::sections (partial map #(section-fn % cat))))))]
     (update file ::sections (partial map #(section-fn % default))))))
