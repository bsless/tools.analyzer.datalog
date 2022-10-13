(ns bsless.tools.analyzer.datalog
  (:require
   [datascript.core :as d]
   [clojure.tools.analyzer.ast :as ast]
   [clojure.tools.analyzer.ast.query :as ast.query]
   [clojure.tools.analyzer.passes.index-vector-nodes :refer [index-vector-nodes]]
   [clojure.tools.analyzer.jvm :as ana.jvm]
   [clojure.string :as str]))

(def common
  #{:op :form :env :raw-forms :top-level :tag :o-tag :ignore-tag :loops :metadata})

(defn qualify
  [n k]
  (keyword (name n) (name k)))

(def core'
  (ana.jvm/analyze-ns 'clojure.core))

(defn qualify-node
  [{:keys [op] :as ast}]
  (->> ast
       (reduce-kv
        (fn [m k v]
          (cond
            (identical? :children k) (assoc! m k (mapv (partial qualify op) v))
            (common k) m
            :else (assoc! (dissoc! m k) (qualify op k) v)))
        (transient ast))
       persistent!))

(defn fix-meta
  [{:keys [meta] :as ast}]
  (cond-> ast
    (and meta (not (:op meta)))
    (-> (dissoc :meta) (assoc :metadata meta))))

(defn fix-const
  [ast]
  (cond-> ast
    (identical? :const (:op ast))
    (-> (dissoc :val) (assoc :value (:val ast)))))

(def qualified-core
  (into
   []
   (map #(ast/prewalk (ast/postwalk % qualify-node) index-vector-nodes))
   core'))

(comment ;; doesn't work
  (def core
    (into
     []
     (map #(ast/prewalk (ast/postwalk % (comp fix-const fix-meta)) index-vector-nodes))
     core')))

(def counter (atom (inc Integer/MIN_VALUE)))
(def cache  (atom {}))
(def rcache (atom {}))

(defn replace-with-eid
  [e]
  (if-let [e' (get @cache e)]
    e'
    (let [e' (swap! counter inc)]
      (swap! cache assoc e e')
      (swap! rcache assoc e' e)
      e')))

(defn prepare-datoms
  [datoms]
  (->> datoms
       (into []
             (map (fn [[e a v]]
                    (let [v (if (nil? v) ::nil v)
                          e' (if (map? e)
                               (replace-with-eid e)
                               e)]
                      [e' a v]))))
       (into []
             (mapcat (fn [[e a v]]
                       (let [v (or (get @cache v) v)]
                         (if (and (vector? v) (not= :form a))
                           (map (fn [v] [:db/add e a v]) v)
                           [[:db/add e a v]])))))))

(def raw-datoms (->> qualified-core ast.query/db))
(def datoms (->> raw-datoms prepare-datoms))

(def schema
  (->> raw-datoms
       (into {:children {:db/cardinality :db.cardinality/many}}
             (comp
              (filter (comp #{:children} second))
              (map peek)
              cat
              (distinct)
              (map (fn [k]
                     (let [n (name k)]
                       [k (cond-> {:db/valueType :db.type/ref
                                   :db/isComponent true}
                            (and (not= "class" n) (str/ends-with? n "s"))
                            (assoc :db/cardinality :db.cardinality/many))])))))))

(def conn (-> schema d/create-conn (doto (d/transact! [[:db/add Integer/MIN_VALUE :db/ident ::nil]]))))

(def _ (d/transact! conn datoms))

(def ret
  (d/q '[:find (pull ?x [*])
         :in $ ?name
         :where
         [?x :op :def]
         [?x :def/name ?name]
         ]
       (d/db conn)
       'get))

(ffirst ret)

;;; Find get-in's children
(d/q '[:find ?ch
       :in $ ?name
       :where
       [?x :op :def]
       [?x :children ?child]
       [?x :def/name ?name]
       [?x ?child ?ch]]
     (d/db conn)
     'get-in)


(def rules
  '[[(calls? ?fn ?var-name ?var)
     [?fn :invoke/fn ?var]
     [?var :op :var]
     [?var :form ?var-name]]
    [(calls? ?fn ?var-name ?var)
     [?fn :invoke/args ?var]
     [?var :op :var]
     [?var :form ?var-name]]
    [(calls? ?fn ?var-name ?var)
     [?fn :children ?child]
     [?fn ?child ?ch]
     (calls? ?ch ?var-name ?var)]])

(d/q '[:find ?x ?var-name ?var
       :in $ % ?name ?var-name
       :where
       [?x :op :def]
       [?x :def/name ?name]
       (calls? ?x ?var-name ?var)
       ]
     (d/db conn)
     rules
     'get-in
     'get)

;;; Find all the functions called by `type`
(d/q '[:find ?x ?var-name ?var
       :in $ % ?name
       :where
       [?x :op :def]
       [?x :def/name ?name]
       (calls? ?x ?var-name ?var)
       ]
     (d/db conn)
     rules
     'type)

;;; Find all the definitions calling `conj`
(d/q '[:find (pull ?x [:op :form])
       :in $ % ?var-name
       :where
       [?var :var/var]
       [?var :form ?var-name]
       [?x :op :def]
       (calls? ?x ?var-name ?var)
       ]
     (d/db conn)
     rules
     'conj)
