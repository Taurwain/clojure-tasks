(ns task3.integral)


(defn- parse-args
  [x dx]
  (let [n (long (Math/floor (/ x dx)))]
    [n (- x (* n dx))]))


(defn- trap
  [f a b]
  (* (/ (- b a) 2.0) (+ (f a) (f b))))


(defn naive-integral
  [f dx]
  (fn [x]
    (let [[n] (parse-args x dx)
          nodes (map (fn [i] (* i dx)) (range 0 (inc n)))]
      (+ (reduce + (map (fn [[a b]] (trap f a b)) (partition 2 1 nodes)))
         (trap f (* n dx) x)))))


(defn memoized-integral
  [f dx]
  (let [store (atom {0 0.0})]
    (letfn [(sum-at [i]
              (or (get @store i)
                  (let [v (+ (sum-at (dec i))
                             (trap f (* (dec i) dx) (* i dx)))]
                    (swap! store assoc i v)
                    v)))]
      (fn [x]
        (let [[n] (parse-args x dx)]
          (+ (sum-at n) (trap f (* n dx) x)))))))


(defn lazy-integral
  [f dx]
  (let [partials (->> (iterate (fn [[x _ sum]]
                                 (let [nx (+ x dx)
                                       nfx (f nx)]
                                   [nx nfx (+ sum (trap f x nx))]))
                               [0.0 (f 0.0) 0.0])
                      (map (fn [[_ _ sum]] sum)))]
    (fn [x]
      (let [[n] (parse-args x dx)]
        (+ (nth partials n) (trap f (* n dx) x))))))
