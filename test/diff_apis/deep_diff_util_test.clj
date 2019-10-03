(ns diff-apis.deep-diff-util-test
  (:require [clojure.test :as t]
            [lambdaisland.deep-diff.diff :as deep-diff]
            [diff-apis.deep-diff-util :as dd-util]))

(t/deftest diff?-test
  (t/is (= false (dd-util/diff? {:a 1})))
  (t/is (= true (dd-util/diff? (deep-diff/->Insertion "i a new"))))
  (t/is (= true (dd-util/diff? (deep-diff/->Deletion "i am gone"))))
  (t/is (= true (dd-util/diff? (deep-diff/->Mismatch "old" "new")))))

(t/deftest any-diffs?-test
  (t/is (= false (dd-util/any-diffs? [{:name "ns-test"
                                       :publics [{:name "p-test"
                                                  :members [{:name "mtest"
                                                             :argslist '[[a b c]]}]}]}])))
  (t/is (= true (dd-util/any-diffs? [{:name "ns-test"
                                      :publics [{:name "p-test"
                                                 :members [{:name "mtest"
                                                            :argslist [['a (deep-diff/->Mismatch 'b 'x) 'c]]}]}]}])))
  (t/is (= true (dd-util/any-diffs? [{:name "ns-test"
                                      :publics [{:name "p-test"
                                                 :members [{:name "mtest"
                                                            (deep-diff/->Insertion :argslist) [['a 'b 'x 'c]]}]}]}]))))

(t/deftest unwrap-elem-test
  (t/is (= :not-wrapped (dd-util/unwrap-elem :not-wrapped)))
  (t/is (= :insert-wrapped (dd-util/unwrap-elem (deep-diff/->Insertion :insert-wrapped))))
  (t/is (= :delete-wrapped (dd-util/unwrap-elem (deep-diff/->Deletion :delete-wrapped))))
  (t/is (thrown-with-msg? clojure.lang.ExceptionInfo #"programming error: cannot unwrap mismatch"
                          (dd-util/unwrap-elem (deep-diff/->Mismatch :old :new)))))


(t/deftest find-test
  (t/is (= [:not-wrapped "at all"] (dd-util/find {:a 1 :not-wrapped "at all" :b 2} :not-wrapped)))
  (t/is (= [(deep-diff/->Insertion :insert-wrapped) "insert-val"]
           (dd-util/find {:a 1
                          (deep-diff/->Insertion :insert-wrapped) "insert-val"
                          :b 2} :insert-wrapped)))
  (t/is (= [(deep-diff/->Deletion :delete-wrapped) "delete-val"]
           (dd-util/find {:a 1
                          (deep-diff/->Deletion :delete-wrapped) "delete-val"
                          :b 2} :delete-wrapped)))
  (t/is (nil?
           (dd-util/find {:a 1
                          (deep-diff/->Deletion :delete-wrapped) "delete-val"
                          :b 2} :nope))) )
(t/deftest find-all-test
  (t/is (= [[:b 2][:not-wrapped "at all"]] (dd-util/find-all {:a 1 :not-wrapped "at all" :b 2} [:b :not-wrapped])))
  (t/is (= [[(deep-diff/->Insertion :insert-wrapped) "insert-val"][:a 1]]
           (dd-util/find-all {:a 1
                              (deep-diff/->Insertion :insert-wrapped) "insert-val"
                              :b 2}
                             [:insert-wrapped :z :x :a])))
  (t/is (= []
           (dd-util/find-all {:a 1
                              (deep-diff/->Insertion :insert-wrapped) "insert-val"
                              :b 2}
                             [:nope :never :found]))) )

(t/deftest get-test
  (t/is (= "simple" (dd-util/get {:so "simple"} :so)))
  (t/is (= "w1val" (dd-util/get {(deep-diff/->Insertion :w1key) "w1val"} :w1key)))
  (t/is (= "w2val" (dd-util/get {:w2key (deep-diff/->Deletion "w2val")} :w2key))))


(t/deftest update-test
    (t/is (= {:k1 "new-val"} (dd-util/update {:k1 "val"} :k1 (constantly "new-val"))))
    (t/is (= {(deep-diff/->Insertion :new-key) "updated-val"}
             (dd-util/update {(deep-diff/->Insertion :new-key) "old-val" }
                             :new-key
                             (constantly "updated-val"))))
    (t/is (= (deep-diff/->Deletion {:k3 "updated-dval"})
             (dd-util/update (deep-diff/->Deletion {:k3 "old-dval"})
                             :k3
                             (constantly "updated-dval")))))
(t/deftest has-key-test
  (t/is (not (dd-util/has-key? nil :akey)))
  (t/is (dd-util/has-key? {:akey "aval"} :akey))
  (t/is (not (dd-util/has-key? {:akey "aval"} :nope)))
  (t/is (dd-util/has-key? {(deep-diff/->Insertion :ikey) "ival"} :ikey)))


(t/deftest changes-only-test
  (t/testing "when no diffs, empty"
    (t/is (= [] (dd-util/changes-only [{:name "ns-test"
                                        :publics [{:name "p-test"
                                                   :members [{:name "mtest"
                                                              :argslist '[[a b c]]}]}]}]))))

  (t/testing "when all namespaces new, return entire diff"
    (let [all-is-new (deep-diff/->Insertion [{:name "ns-test"
                                              :publics [{:name "p-test"
                                                         :members [{:name "mtest"
                                                                    :argslist '[[a b c]]}]}]}])]
      (t/is (= all-is-new (dd-util/changes-only all-is-new)))))

  (t/testing "return ns when its publics are entirely missing"
    (t/is (= [{:name "ns2"
               (deep-diff/->Deletion :publics)
               [{:name "ns2-p1"
                 :members [{:name "ns2-p1-m1"
                            :argslist '[[a b c]]}]}]}]
             (dd-util/changes-only [{:name "ns1"
                                     :publics
                                     [{:name "ns1-p1"
                                       :members [{:name "ns1-p1-m1"
                                                  :argslist '[[a b c]]}]}]}
                                    {:name "ns2"
                                     (deep-diff/->Deletion :publics)
                                     [{:name "ns2-p1"
                                       :members [{:name "ns2-p1-m1"
                                                  :argslist '[[a b c]]}]}]} ]))))

  (t/testing "return entire ns when only ns attribute has changed"
    (t/is (= [{:name "ns1"
               :deprecated (deep-diff/->Insertion "1.1.2")
               :publics
               [{:name "ns1-p1"
                 :members [{:name "ns1-p1-m1"
                            :argslist '[[a b c]]}]}]}]
             (dd-util/changes-only [{:name "ns1"
                                     :deprecated (deep-diff/->Insertion "1.1.2")
                                     :publics
                                     [{:name "ns1-p1"
                                       :members [{:name "ns1-p1-m1"
                                                  :argslist '[[a b c]]}]}]}
                                    {:name "ns2"
                                     :publics
                                     [{:name "ns2-p1"
                                       :members [{:name "ns2-p1-m1"
                                                  :argslist '[[a b c]]}]}]} ]))))

  (t/testing "return only publics with changes"
    (t/is (= [{:name "ns1"
               :publics
               [{:name "ns1-p3"
                 :members [{:name "ns1-p1-m1"
                            :argslist [['a (deep-diff/->Mismatch 'a 'x) 'c]]}]} ]}
              {:name "ns3"
               :publics
               [{:name "ns3-p1"
                 :argslist '[[]]
                 (deep-diff/->Insertion :deprecated)  "1.2.3"}]}]
             (dd-util/changes-only [{:name "ns1"
                                     :publics
                                     [{:name "ns1-p1"
                                       :members [{:name "ns1-p1-m1"
                                                  :argslist '[[a b c]]}]}
                                      {:name "ns1-p2"
                                       :members [{:name "ns1-p1-m1"
                                                  :argslist '[[a b c]]}]}
                                      {:name "ns1-p3"
                                       :members [{:name "ns1-p1-m1"
                                                  :argslist [['a (deep-diff/->Mismatch 'a 'x) 'c]]}]} ]}
                                    {:name "ns2"
                                     :publics
                                     [{:name "ns2-p1"
                                       :members [{:name "ns2-p1-m1"
                                                  :argslist '[[a b c]]}]}]}
                                    {:name "ns3"
                                     :publics
                                     [{:name "ns3-p1"
                                       :argslist '[[]]
                                       (deep-diff/->Insertion :deprecated)  "1.2.3"}]}])))))
