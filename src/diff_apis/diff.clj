(ns diff-apis.diff
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.deep-diff-util :as deep-diff-util]))

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

(defn- load-api
  "Load cljdoc-analyzer generated edn from `edn-filename` for `lang` where `lang` is \"clj\" or \"cljs\"."
  [edn-filename lang]
  (api-essentials (edn/read-string (slurp edn-filename)) lang))

(defn- raise-name-as-key
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

(defn- lower-name-map
  "Convert {:name {:name a}} back to {:name a} in `raised-name-map` preserving any special insertion/deletion records from deep-diff."
  [raised-name-map]
  (map (fn [[k v]]
         (if (and (map? k))
           (deep-diff-util/preserve-deep-diff-type k v)
           v))
       raised-name-map))

(defn- lower-names
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

(defn- changes-only
  "Returns only publics with changes in `diff` result.
  This includes all publics for ns when any change has been made to ns level attributes."
  [diff]
  (->> (map (fn [ns]
              (if (or (deep-diff-util/is-diff? ns) (deep-diff-util/any-diffs? (dissoc ns :publics)))
                ns
                (update ns :publics #(filter deep-diff-util/any-diffs? %))))
            diff)
       (filter #(or (deep-diff-util/is-diff? %) (seq (:publics %))))))

(defn- case-insensitive-comparator [a b]
  (let [la (and a (string/lower-case a))
        lb (and b (string/lower-case b))]
    (if (= la lb)
      (compare a b)
      (compare la lb))))



(defn- sort-by-name
  "Sorts all collections of maps with :name by :name.
  Sort is case insensitive with consistent sort order if :name is case sensitive unique across collection
  and all maps in collection have :name."
  [namespaces]
  (clojure.walk/postwalk (fn [x]
                           (if (and (coll? x) (deep-diff-util/diff-aware-get (first x) :name))
                             (sort-by #(deep-diff-util/diff-aware-get % :name) case-insensitive-comparator x)
                             x))
                         namespaces))

(defn diff-edn
  "Returns deep-diff of api `a` and api `b`.
  Use opts to:
  - `:include` `:all` or just `:changed-publics`, defaults to `:changed-publics`
  TODO: implement exclude, will use it to exclude potemkin, I think
  - `:exlude-namespaces` `:none` or a vector of namespaces to exclude, defaults to `:none`"
  ([a b]
   (diff-edn a b {:include :changed-publics}))
  ([a b opts]
   (-> (deep-diff/diff (raise-name-as-key a) (raise-name-as-key b))
       (lower-names)
       (#(if (= :changed-publics (:include opts)) (changes-only %) %))
       (sort-by-name))))


(defn diff-files
  [a b opts]
  (diff-edn (apply load-api a) (apply load-api b) opts))
