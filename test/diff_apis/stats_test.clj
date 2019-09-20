(ns diff-apis.stats-test
  (:require [clojure.test :as t]
            [lambdaisland.deep-diff.diff :as deep-diff]
            [diff-apis.stats :as stats]))

(t/deftest arglists-test
  (t/is (= [{:type :arglists   :equal 1 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 1 :deleted 0 :inserted 0 :changed 0}
            {:type :namespaces :equal 1 :deleted 0 :inserted 0 :changed 0}]
           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists '[[a b c]]}]}])))

  (t/is (= [{:type :arglists   :equal 3 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 1 :deleted 0 :inserted 0 :changed 0}
            {:type :namespaces :equal 1 :deleted 0 :inserted 0 :changed 0}]
           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists '[[a b c] [d e] []]}]}])))

  (t/is (= [{:type :arglists   :equal 5 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 2 :deleted 0 :inserted 0 :changed 0}
            {:type :namespaces :equal 2 :deleted 0 :inserted 0 :changed 0}]
           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists '[[a b c] [d e] []]}]}
                               {:name "ns2"
                                :publics [{:name "p2"
                                           :members [{:name "m1"
                                                      :arglists '[[x y z] [x]]}]}]}])))

  (t/is (= [{:type :arglists   :equal 2 :deleted 0 :inserted 3 :changed 0}
            {:type :publics    :equal 1 :deleted 0 :inserted 0 :changed 1}
            {:type :namespaces :equal 1 :deleted 0 :inserted 0 :changed 1}]

           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists (deep-diff/->Insertion ['[a b c] '[d e] '[]])}]}
                               {:name "ns2"
                                :publics [{:name "p2"
                                           :members [{:name "m1"
                                                      :arglists '[[x y z] [x]]}]}]}])))


  (t/is (= [{:type :arglists   :equal 1 :deleted 1 :inserted 3 :changed 0}
            {:type :publics    :equal 0 :deleted 0 :inserted 0 :changed 2}
            {:type :namespaces :equal 0 :deleted 0 :inserted 0 :changed 2}]

           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists (deep-diff/->Insertion ['[a b c] '[d e] '[]])}]}
                               {:name "ns2"
                                :publics [{:name "p2"
                                           :members [{:name "m1"
                                                      :arglists ['[x y z] (deep-diff/->Deletion '[x])]}]}]}])))

  (t/is (= [{:type :arglists   :equal 1 :deleted 4 :inserted 3 :changed 0}
            {:type :publics    :equal 0 :deleted 0 :inserted 0 :changed 3}
            {:type :namespaces :equal 0 :deleted 0 :inserted 0 :changed 2}]

           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists (deep-diff/->Insertion ['[a b c] '[d e] '[]])}]}
                               {:name "ns2"
                                :publics [{:name "p2"
                                           :members [{:name "m1"
                                                      :arglists ['[x y z] (deep-diff/->Deletion '[x])]}]}
                                          {:name "p3"
                                           (deep-diff/->Deletion :arglists) '[[a b c] [d e] []]}]}
                               ])))

  (t/is (= [{:type :arglists   :equal 0 :deleted 4 :inserted 3 :changed 1}
            {:type :publics    :equal 0 :deleted 0 :inserted 0 :changed 3}
            {:type :namespaces :equal 0 :deleted 0 :inserted 0 :changed 2}]
           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1"
                                           :arglists (deep-diff/->Insertion ['[a b c] '[d e] '[]])}]}
                               {:name "ns2"
                                :publics [{:name "p2"
                                           :members [{:name "m1"
                                                      :arglists [['x (deep-diff/->Mismatch 'y 'other-y) 'z] (deep-diff/->Deletion '[x])]}]}
                                          {:name "p3"
                                           (deep-diff/->Deletion :arglists) '[[a b c] [d e] []]}]}
                               ]))))

(t/deftest publics-test
  (t/is (= [{:type :arglists   :equal 0 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 4 :deleted 0 :inserted 0 :changed 0}
            {:type :namespaces :equal 3 :deleted 0 :inserted 0 :changed 0}]
           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1-1"
                                           :type "var"}]}
                               {:name "ns2"
                                :publics [{:name "p2-1"
                                           :type "protocol"
                                           :members [{:name "m1"}]}
                                          {:name "p2-2"
                                           :type "var"} ]}
                               {:name "ns3"
                                :publics [{:name "p3-1"
                                           :type "var"}]}])))

  (t/is (= [{:type :arglists   :equal 0 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 3 :deleted 0 :inserted 1 :changed 0}
            {:type :namespaces :equal 2 :deleted 0 :inserted 0 :changed 1}]
           (stats/count-diffs [{:name "ns1"
                                :publics [(deep-diff/->Insertion {:name "p1-1"
                                                                       :type "var"})]}
                               {:name "ns2"
                                :publics [{:name "p2-1"
                                                 :type "protocol"
                                           :members [{:name "m1"}]}
                                          {:name "p2-2"
                                           :type "var"} ]}
                                     {:name "ns3"
                                      :publics [{:name "p3-1"
                                                 :type "var"}]}])))

  (t/is (= [{:type :arglists   :equal 0 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 1 :deleted 2 :inserted 1 :changed 0}
            {:type :namespaces :equal 1 :deleted 0 :inserted 0 :changed 2}]
           (stats/count-diffs [{:name "ns1"
                                :publics [(deep-diff/->Insertion {:name "p1-1"
                                                                  :type "var"})]}
                               {:name "ns2"
                                (deep-diff/->Deletion :publics) [{:name "p2-1"
                                                                        :type "protocol"
                                                                  :members [{:name "m1"}]}
                                                                 {:name "p2-2"
                                                                  :type "var"} ]}
                               {:name "ns3"
                                :publics [{:name "p3-1"
                                           :type "var"}]}])))


  (t/is (= [{:type :arglists   :equal 0 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 0 :deleted 2 :inserted 1 :changed 1}
            {:type :namespaces :equal 0 :deleted 0 :inserted 0 :changed 3}]
           (stats/count-diffs [{:name "ns1"
                                :publics [(deep-diff/->Insertion {:name "p1-1"
                                                                  :type "var"})]}
                               {:name "ns2"
                                (deep-diff/->Deletion :publics) [{:name "p2-1"
                                                                        :type "protocol"
                                                                  :members [{:name "m1"}]}
                                                                 {:name "p2-2"
                                                                  :type "var"} ]}
                               {:name "ns3"
                                :publics [{:name "p3-1"
                                           :type "var"
                                           (deep-diff/->Deletion :no-doc) true}]}])))


 (t/is (= [{:type :arglists   :equal 0 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 0 :deleted 2 :inserted 1 :changed 1}
            {:type :namespaces :equal 0 :deleted 0 :inserted 0 :changed 3}]
           (stats/count-diffs [{:name "ns1"
                                :publics [(deep-diff/->Insertion {:name "p1-1"
                                                                  :type "var"})]}
                               {:name "ns2"
                                :publics (deep-diff/->Deletion [{:name "p2-1"
                                                                 :type "protocol"
                                                                 :members [{:name "m1"}]}
                                                                {:name "p2-2"
                                                                 :type "var"} ])}
                               {:name "ns3"
                                :publics [{:name "p3-1"
                                           :type "var"
                                           (deep-diff/->Deletion :no-doc) true}]}]))) )

(t/deftest namespace-test

  (t/is (= [{:type :arglists   :equal 0 :deleted 0 :inserted 0 :changed 0}
            {:type :publics    :equal 4 :deleted 0 :inserted 0 :changed 0}
            {:type :namespaces :equal 2 :deleted 0 :inserted 0 :changed 1}]
           (stats/count-diffs [{:name "ns1"
                                :publics [{:name "p1-1"
                                           :type "var"}]}
                               {:name "ns2"
                                :deprecated (deep-diff/->Mismatch "2.1" "2.1.1")
                                :publics [{:name "p2-1"
                                           :type "protocol"
                                           :members [{:name "m1"}]}
                                          {:name "p2-2"
                                           :type "var"} ]}
                               {:name "ns3"
                                :publics [{:name "p3-1"
                                           :type "var"
                                           :no-doc true}]}]) )))
