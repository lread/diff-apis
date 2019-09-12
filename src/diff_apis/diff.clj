(ns diff-apis.diff
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [lambdaisland.deep-diff :as deep-diff])
  (:import [lambdaisland.deep_diff.diff Deletion Insertion]))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn- api-essentials
  "Load api for signature comparison. We also include metadata of interest.
  What am I excluding really? :doc :file :line"
  [m lang]
  (walk/postwalk #(if (map? %)
                    (select-keys % [:name :arglists :members :type :dynamic :publics :deprecated :no-doc :skip-wiki])
                    %)
                 (get-in m [:codox lang])))

(defn load-api
  "Load cljdoc-analyzer generated edn from `edn-filename` for `lang` where `lang` is \"clj\" or \"cljs\"."
  [edn-filename lang]
  (api-essentials (edn/read-string (slurp edn-filename)) lang))

(defn raise-name-as-key
  "As preparation for diffing convert [{:name a} {:name b}] to {a {:name a} {b {:name b}}} throughout `api`"
  [api]
  (walk/postwalk
   (fn [x]
     (if (and (list? x) (:name (first x)))
       (reduce (fn [acc m]
                 (assoc acc (:name m) m))
               {}
               x)
       x))
   api))

(defn- preserve-deep-diff-type
  "Preserving the deep-diff type keeps pretty printing with deep-diff pretty wrt insertions an deletions."
  [from-elem new-elem]
  (cond
    (= (type from-elem) Deletion) (new Deletion new-elem)
    (= (type from-elem) Insertion) (new Insertion new-elem)
    :else (throw (ex-info (str "was expecting lambdaisland deep diff deletion or insertion but got:" from-elem) {}))))

(defn lower-name-map
  "Convert {:name {:name a}} back to {:name a} in `raised-name-map` preserving any special insertion/deletion records from deep-diff."
  [raised-name-map]
  (map (fn [[k v]]
         (if (and (map? k))
           (preserve-deep-diff-type k v)
           v))
       raised-name-map))

(defn lower-names
  "Lower names raised for diffing throughout `diff` result."
  [diff]
  (walk/postwalk
   (fn [x]
     (if (and (map? x)
              (map? (first (vals x)))
              (:name (first (vals x))))
       (lower-name-map x)
       x))
   diff))

;; TODO use deep-diff types, wrote this before I realized they were present
(defn is-diff?
  "Returns true if `x` is a deep-diff marker."
  [x]
  (and (map? x) (or (:+ x) (:- x))))

(defn any-diffs?
  "Returns true if any deep-diff markers occur anywhere in `x`."
  [x]
  ;; TODO: maybe using atom not the most elegant way?
  (let [diffs-found (atom false)]
    (walk/postwalk
     (fn [n]
       (when (and (is-diff? n) (= false @diffs-found))
         (reset! diffs-found true))
       n)
     x)
    @diffs-found))

(defn changes-only
  "Returns only publics with changes in `diff` result.
  This includes all publics for ns when any change has been made to ns level attributes."
  [diff]
  (->> (map (fn [ns]
              (if (or (is-diff? ns) (any-diffs? (dissoc ns :publics)))
                ns
                (update ns :publics #(filter any-diffs? %))))
            diff)
       (filter #(or (is-diff? %) (seq (:publics %))))))

(defn- case-insensitive-comparator [a b]
  (let [la (and a (string/lower-case a))
        lb (and b (string/lower-case b))]
    (if (= la lb)
      (compare a b)
      (compare la lb))))

(defn- diff-aware-get [x key]
  (if (is-diff? x)
    (get (first (vals x)) key)
    (get x key)))

(defn- sort-by-name
  "Sorts all collections of maps with :name by :name.
  Sort is case insensitive with consistent sort order if :name is case sensitive unique across collection
  and all maps in collection have :name."
  [namespaces]
  (clojure.walk/postwalk (fn [x]
                           (if (and (coll? x) (diff-aware-get (first x) :name))
                             (sort-by #(diff-aware-get % :name) case-insensitive-comparator x)
                             x))
                         namespaces))

(defn diff-api
  "Returns deep-diff of api `a` and api `b`.
  Use opts to:
  - `:include-changes` `:all` or just `:publics`, defaults to `:changed-publics`
  TODO: implement exclude, will use it to exclude potemkin, I think
  - `:exlude-namespaces` `:none` or a vector of namespaces to exclude, defaults to `:none`"
  ([a b]
   (diff-api a b {:include :changed-publics}))
  ([a b opts]
   (-> (deep-diff/diff (raise-name-as-key a) (raise-name-as-key b))
       (lower-names)
       (#(if (= :changed-publics (:include opts)) (changes-only %) %))
       (sort-by-name))))

;; TODO move to asciidoc report generation to its own ns
(defn- diff-marker-for-asciidoc [elem]
  (if (is-diff? elem)
    (str (when (:+ elem) "➕" )
         (when (:- elem) "➖" ))))

(defn- asciidoc-code [text]
  (str "`+" text "+`"))

(defn- elem-with-markers [elem]
  (if (is-diff? elem)
    (str (when (:+ elem) (str "➕" (asciidoc-code (:+ elem))))
         (when (:- elem) (str "➖" (asciidoc-code (:- elem)))))
    (asciidoc-code elem)))

(defn- attribute-to-asciidoc [m k]
  (if-let [v (diff-aware-get m k)]
    (str (diff-marker-for-asciidoc v) (elem-with-markers v))))

(defn- labeled-attribute-to-asciidoc [m k]
  (if-let [v (attribute-to-asciidoc m k)]
    (str (name k) " " v)))

(defn- asciidoc-arglists [depth arglists]
  (into []
        (map (fn [als]
               [depth (str "["
                           (string/join "," (map #(elem-with-markers %) als))
                           "]")])
             arglists)))

(defn- asciidoc-ns [ns]
  [[1 (str (diff-marker-for-asciidoc ns) (attribute-to-asciidoc ns :name))]
   [2 (labeled-attribute-to-asciidoc ns :no-doc)]
   [2 (labeled-attribute-to-asciidoc ns :no-wiki)]
   [2 (labeled-attribute-to-asciidoc ns :deprecated)]])

(defn- asciidoc-publics [depth p]
  (into [[depth (str (diff-marker-for-asciidoc p) (attribute-to-asciidoc p :name))]]
        (into
         (asciidoc-arglists (inc depth) (p :arglists))
         [[(inc depth) (labeled-attribute-to-asciidoc p :type)]
          [(inc depth) (labeled-attribute-to-asciidoc p :no-doc)]
          [(inc depth) (labeled-attribute-to-asciidoc p :no-wiki)]
          [(inc depth) (labeled-attribute-to-asciidoc p :deprecated)]])))

;; TODO: I suppose, currently this is really returning a nested list report, might be a nice generalization
(defn as-asciidoc-struct [diff]
  (->>
   (reduce (fn [d ns]
             (into d
                   (into (asciidoc-ns ns)
                         (reduce (fn [dp p]
                                   (into dp
                                         (into (asciidoc-publics 2 p)
                                               (reduce (fn [dm m]
                                                         (into dm (asciidoc-publics 3 m)))
                                                       []
                                                       (diff-aware-get p :members)))))
                                 []
                                 (diff-aware-get ns :publics)))))
           []
           diff)
   (filter #(second %))))

(defn struct-to-asciidoc [list-struct]
  (reduce (fn [t [depth text]]
            (str t
                 (apply str (repeat depth "*"))
                 " " text "\n"))
          ""
          list-struct))

(defn as-asciidoc [diff]
  (-> (as-asciidoc-struct diff)
      (struct-to-asciidoc)))

(comment
  (apply str (repeat 4 "*"))

  (new (type d1) "parsley")
  (:- d1)
  (preserve-deep-diff-type d1 "parsley")

  (= (type d1) lambdaisland.deep_diff.diff.Deletion)
  (= (type d1) Deletion)
  )
(comment
  (asciidoc-arglists 5 tal)

  (spit "g3-test.adoc" (as-asciidoc g3))
  (def tal(-> (nth g3 1)
              :publics
              (nth 0)
              :arglists))

  (new (type d1) "booya")
  (deep-diff/diff [1 2 3] [1 2 4])
  (deep-diff/diff {:a 1} {:b 2})
  (deep-diff/diff {:a 1} {:a 2})

  ;; goal 1 diff rewrite-clj against rewrite-cljs
  (def rewrite-clj (load-api "rewrite-clj-0.6.1.edn" "clj"))
  (def rewrite-cljs (load-api "rewrite-cljs-0.4.4.edn" "cljs"))

  (def g1 (diff-api rewrite-clj rewrite-cljs))
  (deep-diff/pretty-print g1)

  ;; goal 2 diff rewrite-cljc clj with rewrite-cljc cljs
  (def rewrite-cljc-clj (load-api "rewrite-cljc-1.0.0-alpha.edn" "clj"))
  (def rewrite-cljc-cljs (load-api "rewrite-cljc-1.0.0-alpha.edn" "cljs"))

  (def g2 (diff-api rewrite-cljc-clj rewrite-cljc-cljs))

  (deep-diff/pretty-print g2)

  ;; goal 3 diff rewrite-clj with rewrite-cljc clj
  (def g3 (diff-api rewrite-clj rewrite-cljc-clj))
  (deep-diff/pretty-print g3)


  )
