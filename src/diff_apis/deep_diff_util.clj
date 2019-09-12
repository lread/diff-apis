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


;; A map can be wrapped with an insertion or deletion record
;; a key can be wrapped with an insertion or deletion record
(defn- diff-aware-key-get [x key]
  (or (get x {:- key})
      (get x {:+ key})
      (get x key)))

(defn- diff-aware-find [x key]
  (or (find x {:- key})
      (find x {:+ key})
      (find x key)))

(defn diff-aware-get [x key]
  (if (is-diff? x)
    (get (first (vals x)) key)
    (diff-aware-key-get x key)))

(defn unwrap-elem [x]
  (if (is-diff? x)
    (val (first x))
    x))

;; we might be dealing with a wrapped map or a wrapped key
(defn diff-aware-update [m k f]
  (let [rm (unwrap-elem m)
        [rk _rv] (find rm k)
        nrm (update rm rk f)]
    (if (= m rm)
      nrm
      (preserve-deep-diff-type m nrm))))




(comment

  (val {:a 1})

  (unwrap-elem {:- :b})
  (unwrap-elem :b)

  (def t {:a 1
          {:- :b} 2})

  (diff-aware-get t :b)

   )
