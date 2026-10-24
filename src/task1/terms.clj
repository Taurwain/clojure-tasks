(ns task1.terms)

(defn variable [x]
  [:var x])
(defn lambda [x body]
  [:lambda x body])
(defn app [f x]
  [:app f x])

(defn variable? [term]
  (= :var (first term)))

(defn lambda? [term]
  (= :lambda (first term)))

(defn app? [term]
  (= :app (first term)))
