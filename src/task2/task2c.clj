(ns task2.task2c)


(defn parse-factor [tokens]
  [(first tokens) (rest tokens)])

(defn parse-term-tail [acc tokens]
  (if (and tokens (or (= '* (first tokens)) (= '/ (first tokens))))
    (recur (list (first tokens) acc (first (rest tokens)))
           (rest (rest tokens)))
    [acc tokens]))

(defn parse-term [tokens]
  (let [[acc rest] (parse-factor tokens)]
    (parse-term-tail acc rest)))

(defn parse-expr [acc tokens]
  (if (and tokens (or (= '+ (first tokens)) (= '- (first tokens))))
    (let [[term rest] (parse-term (rest tokens))]
      (recur (list (first tokens) acc term) rest))
    [acc tokens]))

(defn to-clojure-form [expr]
  (let [[term tokens] (parse-term expr)]
    (first (parse-expr term tokens))))

(defn eval-expression [expr]
  (eval (to-clojure-form expr)))


(eval-expression '(1 + 4 * 3 - 5 / 2))
;; => 21/2

(eval-expression '(2 + 3 * 4))
;; => 14

(eval-expression '(10 - 3 - 2))
;; => 5

(eval-expression '(8 / 4 * 2))
;; => 4

(eval-expression '(1 / 0 + 1))
;; => Execution error: Divide by zero
