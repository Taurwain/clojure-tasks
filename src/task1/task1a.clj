(ns task1.task1a
  (:require [task1.terms :refer [variable
                                  lambda
                                  app
                                  variable?
                                  lambda?
                                  app?]]))


(defn free-vars [term]
  (cond
    (variable? term) #{(second term)}
    (lambda? term) (disj (free-vars (nth term 2)) (second term))
    (app? term) (into (free-vars (second term)) (free-vars (nth term 2)))
    )
)


(free-vars (variable 'x))
;; => #{x}

(free-vars
 (lambda 'x
         (variable 'x)))
;; => #{}

(free-vars
 (lambda 'x
         (app
          (variable 'x)
          (variable 'y))))
;; => #{y}

(free-vars (app (variable 'x) (variable 'y)))
;; => #{x y}

(free-vars (app (variable 'x) (lambda 'x 
                                 (app (variable 'x) (variable 'y)))))
;; => #{x y}
