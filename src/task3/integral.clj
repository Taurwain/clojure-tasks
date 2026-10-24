(ns task3.integral)

(defn- parse-args
  "Раскладывает x = n*dx + r, где 0 <= r < dx. Возвращает [n r]."
  [x dx]
  (let [n (long (Math/floor (/ x dx)))]
    [n (- x (* n dx))]))

(defn- trap
  "Площадь одной трапеции на отрезке [a, b]."
  [f a b]
  (* (/ (- b a) 2.0) (+ (f a) (f b))))

;; ---------------------------------------------------------------------------
;; 3.0. Наивная реализация — при каждом вызове вся сумма считается заново.
;; ---------------------------------------------------------------------------
(defn naive-integral
  "Возвращает F(x) = в∫0^x f(t) dt. При каждом вызове вся сумма считается заново."
  [f dx]
  (fn [x]
    (let [[n r] (parse-args x dx)
          nodes (map (fn [i] (* i dx)) (range 0 (inc n)))]
      (+ (reduce + (map (fn [[a b]] (trap f a b)) (partition 2 1 nodes)))
         (trap f (* n dx) x)))))

;; ---------------------------------------------------------------------------
;; 3.1. Мемоизация промежуточных значений интеграла на сетке интегрирования.
;; ---------------------------------------------------------------------------
(defn memoized-integral
  "Мемоизирует частичные суммы S(i) = в∫0^(i*dx) f(t) dt на сетке с шагом dx.

  При запросе F(x) берётся уже накопленная S(n) для n = floor(x/dx), а вклад
  последнего неполного отрезка [n*dx, x] добавляется трапецией той же точности."
  [f dx]
  (let [store (atom {0 0.0})]
    (letfn [(sum-at [i]
              (or (get @store i)
                  (let [v (+ (sum-at (dec i))
                             (trap f (* (dec i) dx) (* i dx)))]
                    (swap! store assoc i v)
                    v)))]
      (fn [x]
        (let [[n r] (parse-args x dx)]
          (+ (sum-at n) (trap f (* n dx) x)))))))

;; ---------------------------------------------------------------------------
;; 3.2. Бесконечная ленивая последовательность частичных решений.
;; ---------------------------------------------------------------------------
(defn lazy-integral
  "Строит бесконечную ленивую последовательность S(0), S(dx), S(2*dx), ...
  и использует её для вычисления F(x), в том числе для x вне узлов сетки.

  Состояние шага — [x (f x) sum]. Ленивая реализация кэширует уже вычисленные
  элементы, поэтому повторные запросы не пересчитывают пройденные трапеции."
  [f dx]
  (let [partials (->> (iterate (fn [[x fx sum]]
                                 (let [nx (+ x dx)
                                       nfx (f nx)]
                                   [nx nfx (+ sum (trap f x nx))]))
                               [0.0 (f 0.0) 0.0])
                      (map (fn [[_ _ sum]] sum)))]
    (fn [x]
      (let [[n r] (parse-args x dx)]
        (+ (nth partials n) (trap f (* n dx) x))))))



(defn run-examples
  "Запускает демонстрацию трёх реализаций интеграла."
  []
  (let [f (fn [t] (* t t))
        exact (fn [x] (/ (* x x x) 3.0))
        dx 0.1
        points [0.5 1.0 1.25 2.0]
        implementations
        [["naive-integral" naive-integral]
         ["memoized-integral" memoized-integral]
         ["lazy-integral" lazy-integral]]]

    (println "\n=== Численное интегрирование f(t) = t^2 ===")
    (println "Точный интеграл: F(x) = x^3 / 3")
    (println "Шаг dx =" dx)

    (doseq [[name make-integral] implementations]
      (let [F (make-integral f dx)]
        (println "\nРеализация:" name)
        (doseq [x points]
          (println
           (format "x = %.2f | F(x) = %.6f | точное = %.6f | ошибка = %.6f"
                   (double x)
                   (double (F x))
                   (double (exact x))
                   (Math/abs (- (double (F x))
                                (double (exact x))))))))

      (println "\n=== Демонстрация завершена ==="))))

(run-examples)