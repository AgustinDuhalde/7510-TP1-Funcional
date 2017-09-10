(ns logical-interpreter)
(require '[clojure.string :as str])

(defprotocol Query (imply-query? [this query]))

(defn get-query-name
  "Return query name from a query"
  [query]
  (def index-start (str/index-of query "("))
  (subs query 0 index-start)
  )

(defn get-query-params
  "Return query params from a query"
  [query]
  (def index-start (str/index-of query "("))
  (def index-end (str/index-of query ")"))
  (subs query (+ index-start 1) index-end)
  )

(defn get-rule-params
  "Return rule params from a rule"
  [rule]
  (def index-start (str/index-of rule "("))
  (def index-end (str/index-of rule ")"))
  (subs rule (+ index-start 1) index-end)
  )

(defn get-rule-facts
  "Return rule facts from a rule"
  [rule]
  (def rule-facts (str/split rule #" :- "))
  (def rule-facts (get rule-facts 1))
  (str/join "" (drop-last rule-facts))
  )

(defn create-params-map
  "Create a map with rule params as keys and query params as values"
  [query-params rule-params]
  (def query-params-list (str/split query-params #", "))
  (def rule-params-list (str/split rule-params #", "))
  (zipmap rule-params-list query-params-list)
  )

(defn replace-params
  "Replace key for value in a rule"
  [rule [k v]]
  (str/replace rule k v)
  )

(defrecord Facts [facts]
  Query
  (imply-query? [this query]

    (def results
      (filter
        (fn [fact] (str/includes? fact query))
        (:facts this)))

    (not (empty? results))
    )
  )

(defrecord Rules [rules facts]
  Query
  (imply-query? [this query]
    (def query-name (get-query-name query))
    (def query-params (get-query-params query))

    (def rule (get rules query-name))
    (def rule-params (get-rule-params rule))
    (def rule-facts (get-rule-facts rule))

    (def params-map (create-params-map query-params rule-params))
    (def replaced-facts (reduce replace-params rule-facts params-map))

    (def replaced-facts (str/split replaced-facts #"\), "))
    (def query-results (map (fn [fact] (imply-query? facts fact)) replaced-facts))
    (every? true? query-results)
    )
  )

(defn fact?
  "Return true if it is a fact"
  [line]
  (not (str/includes? line ":-"))
  )

(defn rule?
  "Return true if it is a rule"
  [line]
  (str/includes? line ":-")
  )

(defn database-complete?
  "Return true if the database is complete"
  [lines]
  (def result
    (filter (fn [line] (not (str/includes? line "."))) lines))
  (empty? result)
  )

(defn query-complete?
  "Return true if the query is complete"
  [query]
  (and
    (not (empty? query))
    (str/includes? query "(")
    (str/includes? query ")")
    )
  )

(defn create-facts
  "Return Facts record"
  [lines]
  (def facts-data (filter fact? lines))
  (new Facts facts-data)
  )

(defn create-rules
  "Return Rules record"
  [lines facts]
  (def rules-data (filter rule? lines))
  (def rules-keys (mapcat (fn [rule] (str/split rule #"\(.*")) rules-data))
  (def rules-keys (map (fn [key] (str/triml key)) rules-keys))
  (def rules-map (zipmap rules-keys rules-data))
  (new Rules rules-map facts)
  )

(defn check-query
  "Create Facts and Rules, and check query implication"
  [lines query]
  (def query-name (get-query-name query))

  (def facts (create-facts lines))
  (def rules (create-rules lines facts))

  (if (contains? rules-map query-name)
    (imply-query? rules query)
    (imply-query? facts query))
  )

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def allLines (str/split-lines database))
  (def lines (filter not-empty allLines))

  (if (and (query-complete? query) (database-complete? lines))
    (check-query lines query)
    nil)
  )