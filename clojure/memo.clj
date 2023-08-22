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

; memo :: (T -> U) -> (T -> U)
; memoization (cache) decorator
(defn memo [f]
  (let [cache {}]
    (fn [arg] (cond (contains? cache arg) (arg cache) ; if cache contains the passed argument return cached value
                    :else (let [res (f arg)]          ; otherwise...
                            (update cache arg res)    ; add value to cache
                            res)))))                  ; and return value
