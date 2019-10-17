(ns diff-apis.report.asciidoc.header
  (:require [clojure.string :as string]
            [diff-apis.report.asciidoc.render :as render]))

(defn- project-as-code [p]
  (str "`+" (:project p) "+` `+" (:version p) "+` `+" (:lang p) "+`"))

(defn- project-as-text [p]
  (str "++" (:project p) " " (:version p) " " (:lang p) "++"))

(defn header-title [{:keys [projects]}]
  ["// This file was auto-generated by diff-apis, best not to edit"
   (str  "= Diff of "  (project-as-text (:a projects)) " & "  (project-as-text (:b projects)))
   ":toc: macro"
   ":toclevels: 5"
   ":!toc-title:"
   ""])

(defn- diff-overview [{:keys [projects]}]
  ["**Diff of apis in:**"
   ""
   (str  "A. "  (project-as-code (:a projects)))
   (str "B. "  (project-as-code (:b projects)))
   ""])

(defn- run-args [{:keys [run-args]}]
  (-> (into ["**Options**:"
             ""
             "|==="
             "| Option | Value"
             ""])
      (into (flatten (map (fn [[k v]]
                            [[(str "l|" k)]
                             [(str "l|" v)]])
                          (:opts run-args))))
      (into [ "|==="])))

(defn- legend []
  ["**Legend:**"
   ""
   (str "* " (render/change-prefix :-)  (render/change-text :- "A only" ))
   (str "* " (render/change-prefix :+)  (render/change-text :+ "B only"))
   (str "* " (render/change-prefix :-)  (render/change-text :- "A is" )
        (render/change-prefix :+) (render/change-text :+ "different from B"))
   (str "* " (render/change-prefix nil) (render/change-text nil "changes within A and B"))
   (str "* " (render/change-prefix :=)  (render/change-text := "equal"))
   ""])

(defn- stat-row [stat]
  [(str "| " (name (:type stat)))
   (str "| " (:changed stat))
   (str "| " (:deleted stat))
   (str "| " (:inserted stat))
   ""])

(defn- stats [{:keys [stats]}]
  (-> (into ["**Stats:**"
             ""
             "|==="
             (str "| Element "
                  "| Have " (render/change-text :- "changes") " " (render/change-text :+ "within") " "
                  "| " (render/change-text :- "In A Only") " "
                  "| " (render/change-text :+ "In B Only"))
             ""])
      (into (flatten (map stat-row stats)))
      (into ["|==="])))

(defn- notes [notes-content]
  (if notes-content
    (-> (into ["**Notes:**"
               ""])
        (into notes-content)
        (into [""]))
    []))

(defn header [result notes-content]
  (string/join "\n"
               (-> (header-title result)
                   (into (diff-overview result))
                   (into (run-args result))
                   (into (legend))
                   (into (stats result))
                   (into (notes notes-content))
                   (into ["**Table of diffs:**"
                          ""
                          "toc::[]"
                          ""]))))
