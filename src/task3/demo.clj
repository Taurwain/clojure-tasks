(ns task3.demo
  (:require [task3.integral :as integral]))


(def implementations
  [["naive-integral"    integral/naive-integral]
   ["memoized-integral" integral/memoized-integral]
   ["lazy-integral"     integral/lazy-integral]])


(defn- counted-f
  [f]
  (let [calls (atom 0)]
    [(fn [t] (swap! calls inc) (f t)) calls]))


(defn- timed
  [thunk]
  (let [t0 (System/nanoTime)
        result (thunk)]
    [(/ (- (System/nanoTime) t0) 1e6) result]))


(defn verify-memoization
  [f dx]
  (let [[f-cnt calls] (counted-f f)]
    (println "\n=== Проверка мемоизации (по числу вызовов f) ===")
    (println (format "шаг сетки dx = %s" (double dx)))
    (doseq [[name make] implementations]
      (reset! calls 0)
      (let [F (make f-cnt dx)]
        (F 2.0)
        (let [after-big @calls]
          (F 1.4)
          (println (format "%-16s вызовов на F(2.0): %-5d  +ещё вызовов на F(1.4): %d"
                           name after-big (- @calls after-big)))))))
  nil)


(defn benchmark
  [f dx reps]
  (println "\n=== Замер времени ===")
  (println (format "dx = %s, повторных тёплых вызовов = %d" (double dx) reps))
  (doseq [[name make] implementations]
    (let [F (make f dx)]
      (let [[cold-ms _] (timed (fn [] (F 2.0)))
            [hot-ms _]  (timed (fn [] (dotimes [_ reps] (F 1.75))))]
        (println (format "%-16s холодный F(2.0): %-9.3f мс | %d x F(1.75): %.3f мс"
                         name cold-ms reps hot-ms)))))
  nil)


(defn run-demo
  [& {:keys [dx reps]}]
  (let [dx
        (or dx 0.1)
        reps (or reps 100000)
        f (fn [t] (* t t))]
    (verify-memoization f dx)
    (benchmark f dx reps)))


(comment
  (task3.demo/verify-memoization (fn [t] (* t t)) 0.1)
  (task3.demo/benchmark         (fn [t] (* t t)) 0.1 100000)
  (task3.demo/run-demo          :dx 0.1 :reps 100000)
)