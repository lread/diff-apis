(ns ^:no-doc diff-apis.deep-diff-util
  (:require [clojure.walk :as walk])
  (:import [lambdaisland.deep_diff.diff Deletion Insertion]))

(defn preserve-deep-diff-type
  "Preserving the deep-diff type keeps pretty printing with deep-diff pretty wrt insertions an deletions."
  [from-elem new-elem]
  (cond
    (= (type from-elem) Deletion) (new Deletion new-elem)
    (= (type from-elem) Insertion) (new Insertion new-elem)
    :else (throw (ex-info (str "was expecting lambdaisland deep diff deletion or insertion but got:" from-elem) {}))))

(defn is-diff?
  "Returns true if `x` is a deep-diff marker."
  [x]
  ;; TODO use deep-diff types, wrote this before I realized they were present
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


(defn diff-aware-get [x key]
  (if (is-diff? x)
    (get (first (vals x)) key)
    (get x key)))
