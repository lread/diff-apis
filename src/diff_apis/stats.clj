(ns diff-apis.stats
  (:require [clojure.zip :as zip]
            [diff-apis.deep-diff-util :as dd-util]))

(defn- in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn- zipper
  "Zipper to navigate into maps and sequentials"
  [diff]
  (zip/zipper
   #(or (map? %) (sequential? %))
   seq
   (fn [p xs]
     (if (isa? (type p) clojure.lang.MapEntry)
       (into [] xs)
       (into (empty p) xs)))
   diff))

(defn- diff-in-path?
  "Returns true if `diff-pred` is anywhere in path `p`. Deep-diff marker can wrap a key to indicate key was inserted or deleted."
  [diff-pred p]
  (some #(or (diff-pred %)
             (and (map-entry? %) (diff-pred (key %))))
        p))

(defn- diff-counter
  "Returns map of counts for `:deleted` `:inserted` `:changed` and `:equal` for elements matching `filter-pred`"
  [filter-pred diff]
  (merge {:deleted 0 :inserted 0 :changed 0 :equal 0}
         (->> (zipper diff)
              (iterate zip/next)
              (take-while #(not (zip/end? %)))
              (filter filter-pred)
              (map (fn [x] (let [n (zip/node x)
                                 p (zip/path x)]
                             (cond
                               (or (dd-util/deleted? n) (diff-in-path? dd-util/deleted? p)) :deleted
                               (or (dd-util/inserted? n) (diff-in-path? dd-util/inserted? p)) :inserted
                               (dd-util/changed? n) :changed
                               :else :equal))))
              (frequencies))))

(defn- unwrapped-lineage
  "Returns list of nodes from `loc` to `root` filtering out any diff wrapper elements."
  [loc]
  (let [ps (->> loc
                (iterate zip/up)
                (take-while identity)
                (map zip/node)
                (remove #(or (dd-util/unary-diff? %)
                             (and (map-entry? %) (in? #{ :- :+ } (key %)))))
                (map #(if (and (map-entry? %) (dd-util/diff? (key %)))
                        (first {(dd-util/unwrap-elem (key %)) (val %)})
                        %)))]
    ps))

(defn count-diffs
  "Returns vector of maps of counts of `:deleted` `:inserted` `:changed` and `:equal` for `:types`: `:arglists` `:publics` `:namespaces`"
  [diff]
  [(merge {:type :arglists}
          (diff-counter #(and (vector? (zip/node %))
                              (let [grandparent (nth (unwrapped-lineage %) 2 nil)]
                                (and (map-entry? grandparent) (= :arglists (key grandparent)))))
                        diff))
   (merge {:type :publics}
          (diff-counter #(and (map? (zip/node %))
                              (let [grandparent (nth (unwrapped-lineage %) 2 nil)]
                                (and (map-entry? grandparent) (= :publics (key grandparent)))))
                        diff))
   (merge {:type :namespaces}
          (diff-counter #(and (map? (zip/node %))
                              (= 2 (count (unwrapped-lineage %))))
                        diff))])
