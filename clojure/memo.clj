(ns cache)

; memo :: (T -> U) -> (T -> U)
; memoization (cache) decorator
(defn memo [f]
  (let [cache (atom {})]
    (fn [arg]
      (cond (contains? @cache arg) (@cache arg) ; if in cache return cached value
            :else (let [res (f arg)]            ; otherwise...
                    (swap! cache assoc arg res) ; add value to cache
                    res)))))                    ; and return value
