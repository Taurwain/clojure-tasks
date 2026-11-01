(ns task4.expr)

(def add-op?   '#{+ -})
(def mult-op?  '#{* /})


(defn parse-factor [tokens]
  [(first tokens) (rest tokens)])


(defn parse-term-tail [acc tokens]
  (if (and tokens (contains? mult-op? (first tokens)))
    (parse-term-tail (list (first tokens) acc (first (rest tokens)))
                     (rest (rest tokens)))
    [acc tokens]))


(defn parse-term [tokens]
  (let [[acc rest] (parse-factor tokens)]
    (parse-term-tail acc rest)))


(defn parse-expr-tail [acc tokens]
  (if (and tokens (contains? add-op? (first tokens)))
    (let [[term rest] (parse-term (rest tokens))]
      (parse-expr-tail (list (first tokens) acc term) rest))
    [acc tokens]))


(defn to-clojure-form
  [expr]
  (let [[term tokens] (parse-term expr)]
    (first (parse-expr-tail term tokens))))


(defn eval-expression
  [expr bindings]
  (let [prefix   (to-clojure-form expr)
        vars     (distinct (filter #(and (symbol? %)
                                         (not (contains? add-op? %))
                                         (not (contains? mult-op? %)))
                                   expr))
        resolved (reduce (fn [acc v]
                           (if (contains? bindings v)
                             (conj acc v (get bindings v))
                             (throw (ex-info (str "Нет значения для " v)
                                             {:var v}))))
                         [] vars)]
    (eval (list 'let resolved prefix))))


(defmacro infix
  [expr]
  (to-clojure-form expr))


;; 4a
(eval-expression '(1 + 4 * 3 - 5 / 2) {})          ;=> 21/2
(eval-expression '(x + 4 * y - 5 / 2) '{x 10, y 4}) ;=> 47/2
(eval-expression '(x - y - 2) '{x 10, y 3})         ;=> 5
(eval-expression '(x / y * 2) '{x 8, y 4})          ;=> 4

;; 4b
(infix (2 + 3 * 4))              ;=> 14
(infix (10 - 3 - 2))             ;=> 5
(let [x 8 y 4] (infix (x / y * 2))) ;=> 4

;; macroexpand-1
(macroexpand-1 '(infix (x + 4 * y - 5 / 2)))
;;=> (- (+ x (* 4 y)) (/ 5 2))

;; ошибка при отсутствии переменной
(eval-expression '(x + 1) {})   ;=> Exception (ex-info)
