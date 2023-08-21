(ns )

; determine frequence of element occurence within sequence
; counter :: [T] -> {T -> number} (hashmap)
(defn counter [seq]
  (let [update-hmap (fn [counts a]
                      (cond (contains? counts a) (update counts a inc)
                            :else (update counts a (fn [] 1))))]
    (reduce update-hmap {} seq)))
