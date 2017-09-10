(ns unit-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]
            [clojure.string :as str]))

(deftest query-complete?-test
  (testing "varon(juan). should be true"
    (is (= (query-complete? "varon(juan).")
           true)))

  (testing "varon should be false"
    (is (= (query-complete? "varon")
           false)))

  (testing "empty query should be false"
    (is (= (query-complete? "")
           false)))

  (testing "varon( query should be false"
    (is (= (query-complete? "varon(")
           false)))

  (testing "varon) query should be false"
    (is (= (query-complete? "varon)")
           false)))
  )

(def incomplete-database "
	varon
")

(def complete-database "
	varon(juan).
")

(def complete-database-allLines (str/split-lines complete-database))
(def complete-database-lines (filter not-empty complete-database-allLines))

(deftest database-complete?-test

  (testing "incomplete database should be false"
    (is (= (database-complete? incomplete-database)
           false)))

  (testing "complete database should be true"
    (is (= (database-complete? complete-database-lines)
           true)))
  )

(deftest fact?-test
  (testing "varon(roberto). should be true"
    (is (= (fact? "varon(roberto).")
           true)))

  (testing "hijo(X, Y) :- varon(X), padre(Y, X). should be false"
    (is (= (fact? "hijo(X, Y) :- varon(X), padre(Y, X)")
           false)))
  )

(deftest rule?-test
  (testing "varon(roberto). should be false"
    (is (= (rule? "varon(roberto).")
           false)))

  (testing "hijo(X, Y) :- varon(X), padre(Y, X). should be true"
    (is (= (rule? "hijo(X, Y) :- varon(X), padre(Y, X)")
           true)))
  )

(deftest get-query-name-test
  (testing "varon(roberto) should return varon"
    (is (= (get-query-name "varon(roberto)")
           "varon")))

  (testing "hijo(roberto, pepe) should return hijo"
    (is (= (get-query-name "hijo(roberto, pepe)")
           "hijo")))
  )

(deftest get-query-params-test
  (testing "varon(roberto) should return roberto"
    (is (= (get-query-params "varon(roberto)")
           "roberto")))

  (testing "hijo(roberto, pepe) should return roberto, pepe"
    (is (= (get-query-params "hijo(roberto, pepe)")
           "roberto, pepe")))
  )

(deftest get-rule-params-test
  (testing "hijo(X, Y) :- varon(X), padre(Y, X). should return X, Y"
    (is (= (get-query-params "hijo(X, Y) :- varon(X), padre(Y, X).")
           "X, Y")))

  (testing "subtract(X, Y, Z) :- add(Y, Z, X). should return X, Y, Z"
    (is (= (get-query-params "subtract(X, Y, Z) :- add(Y, Z, X).")
           "X, Y, Z")))
  )

(deftest get-rule-facts-test
  (testing "hijo(X, Y) :- varon(X), padre(Y, X). should return varon(X), padre(Y, X)"
    (is (= (get-rule-facts "hijo(X, Y) :- varon(X), padre(Y, X).")
           "varon(X), padre(Y, X)")))

  (testing "subtract(X, Y, Z) :- add(Y, Z, X). should return add(Y, Z, X)"
    (is (= (get-rule-facts "subtract(X, Y, Z) :- add(Y, Z, X).")
           "add(Y, Z, X)")))
  )

(deftest create-params-map-test
  (testing "create parent params map should return a map with rule params as keys and query params as values"
    (def query-params-test (get-query-params "hijo(roberto, pepe)"))
    (def rule-params-test (get-rule-params "hijo(X, Y) :- varon(X), padre(Y, X)."))
    (is (= (create-params-map query-params-test rule-params-test)
           {"X" "roberto", "Y" "pepe"})))

  (testing "create number params map should return a map with rule params as keys and query params as values"
    (def query-params-test (get-query-params "subtract(1, 2, 3)"))
    (def rule-params-test (get-rule-params "subtract(X, Y, Z) :- add(Y, Z, X)."))
    (is (= (create-params-map query-params-test rule-params-test)
           {"X" "1", "Y" "2", "Z" "3"})))
  )

(deftest replace-params-test
  (testing "replace parent params should replace key for value in a rule"
    (def params-map-test {"X" "roberto", "Y" "pepe"})
    (def rules-fact-test "varon(X), padre(Y, X)")
    (is (= (reduce replace-params rules-fact-test params-map-test)
           "varon(roberto), padre(pepe, roberto)")))

  (testing "replace number params should replace key for value in a rule"
    (def params-map-test {"X" "1", "Y" "2", "Z" "3"})
    (def rules-fact-test "add(Y, Z, X)")
    (is (= (reduce replace-params rules-fact-test params-map-test)
           "add(2, 3, 1)")))
  )

(deftest facts-test
  (def facts-test (create-facts ["varon(juan)", "varon(pepe)"]))

  (testing "Facts should contains a list of all facts"
    (is (= (:facts facts-test)
           ["varon(juan)", "varon(pepe)"])))

  (testing "Facts should imply varon(juan) query"
    (is (= (imply-query? facts-test "varon(juan)")
           true)))

  (testing "Facts should not imply varon(maria) query"
    (is (= (imply-query? facts-test "varon(maria)")
           false)))
  )

(deftest rules-test
  (def facts-test (create-facts ["varon(juan)", "varon(pepe)", "padre(roberto, pepe)"]))
  (def rules-test (create-rules ["hijo(X, Y) :- varon(X), padre(Y, X)."] facts-test))

  (testing "Rules should contains Facts and a map of rules"
    (is (= (:facts rules-test) facts-test))
    (is (= (:rules rules-test) {"hijo" "hijo(X, Y) :- varon(X), padre(Y, X)."})))

  (testing "Rules should imply hijo(pepe, roberto) query"
    (is (= (imply-query? rules-test "hijo(pepe, roberto)")
           true)))

  (testing "Rules should imply hijo(juan, pepe) query"
    (is (= (imply-query? rules-test "hijo(juan, pepe)")
           false)))
  )
