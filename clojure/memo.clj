(ns )

; memo :: (T -> U) -> (T -> U)
; memoization (cache) decorator
(defn memo [f]
  (let [cache {}]
    (fn [arg] (if (contains? cache arg)    ; if cache contains the passed argument
                  (arg cache)              ; return cached value
                  (let [res (f arg)]       ; otherwise...
                    (update cache arg res) ; add value to cache
                    res)))))               ; and return value
