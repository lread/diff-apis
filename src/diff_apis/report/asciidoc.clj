(ns diff-apis.report.asciidoc
  (:require [clojure.string :as string]
            [diff-apis.deep-diff-util :as deep-diff-util]))

(defn- diff-marker-for-asciidoc [elem]
  (if (deep-diff-util/is-diff? elem)
    (str (when (:+ elem) "➕" )
         (when (:- elem) "➖" ))))

(defn- asciidoc-code [text]
  (str "`+" text "+`"))

(defn- elem-with-markers [elem]
  (if (deep-diff-util/is-diff? elem)
    (str (when (:+ elem) (str "➕" (asciidoc-code (:+ elem))))
         (when (:- elem) (str "➖" (asciidoc-code (:- elem)))))
    (asciidoc-code elem)))

(defn- attribute-to-asciidoc [m k]
  (if-let [v (deep-diff-util/diff-aware-get m k)]
    (str (diff-marker-for-asciidoc v) (elem-with-markers v))))

(defn- labeled-attribute-to-asciidoc [m k]
  (if-let [v (attribute-to-asciidoc m k)]
    (str (name k) " " v)))

(defn- asciidoc-arglists [depth arglists]
  (into []
        (map (fn [als]
               [depth (str "["
                           (string/join " " (map #(elem-with-markers %) als))
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
         (asciidoc-arglists (inc depth) (deep-diff-util/diff-aware-get p :arglists))
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
                                                       (deep-diff-util/diff-aware-get p :members)))))
                                 []
                                 (deep-diff-util/diff-aware-get ns :publics)))))
           []
           diff)
   (filter #(second %))))

(comment
  (into []
        (into [[1 2] [3 4]]
              (into [[5 6] [7 8]])))

  (-> []
      (into [[1 2] [3 4]])
      (into [[5 6] [7 8]]))

  )


(defn struct-to-asciidoc [list-struct]
  (reduce (fn [t [depth text]]
            (str t
                 (apply str (repeat depth "*"))
                 " " text "\n"))
          ""
          list-struct))

(defn header [{:keys [a b opts]}]
  (str "Diff results between apis in:\n\n"
       "* " (asciidoc-code (:project a)) " " (asciidoc-code (:version a)) " " (asciidoc-code (:lang a)) "\n"
       "* " (asciidoc-code (:project b)) " " (asciidoc-code (:version b)) " " (asciidoc-code (:lang b)) "\n\n"
       "With options: " (asciidoc-code opts) "\n\n"))

(defn as-asciidoc [result]
  (str (header (:run-args result))
       (-> (as-asciidoc-struct (:diff result))
           (struct-to-asciidoc))))
