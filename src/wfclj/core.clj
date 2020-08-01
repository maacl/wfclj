(ns wfclj.core
  (:require [clojure.inspector]
            [clojure.set]
            [clojure.pprint]))

;;shared logic
(defn valid-dirs [[x y] [max-x max-y]]
  (cond-> []
    (> x 0) (conj [-1 0])
    (< x (dec max-x)) (conj [1 0])
    (> y 0) (conj [0 -1])
    (< y (dec max-y)) (conj [0 1])))

;; derive compatibillities and weights
(def input-matrix-1
  [[:l :l :l :l]
   [:l :l :l :l]
   [:l :l :l :l]
   [:l :l :l :l]
   [:l :c :c :l]
   [:c :s :s :c]
   [:s :s :s :s]
   [:s :s :s :s]])

(defn dimensions [matrix]
  [(count matrix)
   (count (first matrix))])

(defn derive-weights [matrix]
  (frequencies
   (apply concat matrix)))

(defn rules [matrix dims]
  (into
   #{}
   (apply concat
          (for [[x row] (map-indexed vector matrix)
                [y tile] (map-indexed vector row)]
            (for [[dx dy :as d] (valid-dirs [x y] dims)]
              (let [other-tile (get-in matrix [(+ x dx) (+ y dy)])]
                [tile d other-tile]))))))

(defn derive-compabilities [matrix]
  (let [dims (dimensions matrix)
        flat-rules (rules matrix dims)]
    (reduce (fn [rule-map [tile d other-tile]]
              (update-in rule-map [tile d] (fnil conj #{other-tile}) other-tile))
            {}
            flat-rules)))

(def compabilities
  (derive-compabilities input-matrix-1))

(def weights
  (derive-weights input-matrix-1))

;; main logic
(def compass {[1 0]  :down
              [-1 0] :up
              [0 -1] :left
              [0 1]  :right})

(defn make-grid [x-dim y-dim tiles]
    (into (sorted-map)
          (for [x (range x-dim)
                y (range y-dim)]
            [[x y] [tiles (valid-dirs [x y] [x-dim y-dim])]])))

(defn print-grid [grid cols]
  (clojure.pprint/pprint
   (for [row (partition cols grid)]
     (map (fn [[_ [l _]]] (first l)) row))))

(defn collapsed-val [loc grid]
  (ffirst (get grid loc)))

(defn neighbour [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn collapsed? [loc grid]
  (= 1 (count (first (get grid loc)))))

(defn fully-collapsed? [grid]
  (every? true? (map collapsed? (keys grid) (repeat grid))))

(defn tiles-loc [loc grid]
  (first (get grid loc)))  

(defn swap-possible-tiles [tiles loc grid]
  (assoc-in grid [loc 0] tiles))

;; https://stackoverflow.com/questions/14464011/idiomatic-clojure-for-picking-between-random-weighted-choices
(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #( <= % r ) w)))))

(defn collapse-loc [loc grid]
  (let [choice (weighted-rand-choice
                (select-keys weights (seq (tiles-loc loc grid))))]
    (swap-possible-tiles #{choice} loc grid)))

(defn shannon-entropy [loc grid]
  (let [weights (vals (select-keys weights (seq (tiles-loc loc grid))))
        [sw swlw] (reduce (fn [[sw swlw] w]
                            [(+ sw w) (+ swlw (* w (Math/log w)))])
                          [0 0]
                          weights)]
    (- (Math/log sw) (/ swlw sw))))

(defn kv-for-min-val [m]
  "Returns the [key value] for the minimum v found in m."
  (reduce (fn [[k1 v1]
              [k2 v2]]
            (if (< v2 v1)
              [k2 v2]
              [k1 v1]))
          [nil ##Inf] m))

(defn min-entropy-loc [grid]
  (kv-for-min-val
   (let [ks (remove #(collapsed? % grid) (keys grid))] ;; filter out collapsed locations
     (map (fn [loc g] [loc
                      (- (/ (rand) 1000)
                         (shannon-entropy loc g))])
          ks (repeat grid)))))

(defn remove-from-possible-tiles [tile loc grid]
  (update-in grid [loc 0] #(disj % tile)))

(defn check-compatibility [tile other-tile dir]
  (let [comps (get-in compabilities [tile dir])]
       (contains? comps other-tile)))

(defn propagate [loc grid]
  (let [stack (atom [loc])
        grid (atom grid)]
    (while (not-empty @stack)
      (let [[s l] [(pop @stack) (peek @stack)]
            cur-possible-tiles (tiles-loc l @grid)]
        (reset! stack s)
        (doall
         (for [direction (second (get @grid l))];; we get the precomputed valid-dirs
           (let [n (neighbour l direction)]
             (doall
              (for [other-tile (tiles-loc n @grid)]
                (let [other-tile-is-possible
                      (some true? (map check-compatibility
                                       cur-possible-tiles
                                       (repeat other-tile)
                                       (repeat direction)))]
                  (when (not other-tile-is-possible)
                    (reset! grid (remove-from-possible-tiles other-tile n @grid))
                    (swap! stack conj n))))))))))
    @grid))


(defn check-if-other-tile-compatible-with-current-tiles-in-direction [[grid stack] [tiles other-tile direction neighbour]]
   (let [other-tile-is-possible
         (some true? (map check-compatibility tiles (repeat other-tile) (repeat direction)))]
     (if (not other-tile-is-possible)
       [(remove-from-possible-tiles other-tile neighbour grid)
        (conj stack neighbour)]
       [grid stack])))

(defn alt-propagate [loc grid]
  (loop [grid grid
         stack [loc]]
    (if (empty? stack)
      grid
      (let [loc (peek stack)
            stack (pop stack)
            tiles (tiles-loc loc grid)
            other-tiles (let [directions (second (get grid loc)) ;; we get the precomputed valid-dirs
                              neighbours (map #(neighbour loc %) directions)]
                          (for [[direction neighbour] (partition 2 (interleave directions neighbours))
                                other-tile (tiles-loc neighbour grid)]
                            [tiles other-tile direction neighbour]))
            [grid stack] (reduce check-if-other-tile-compatible-with-current-tiles-in-direction [grid stack] other-tiles)]
        (recur grid stack)))))

(let [[a b] [1 2]]
  (println a b))

;; compare propagate with alt-propagate
(let [grid (make-grid 5 5 #{:s :l :c})
      loc [2 2]
      grid-w-loc-collapsed (swap-possible-tiles #{:c} loc grid)
      grid-propagated-w-propagator (propagate loc grid-w-loc-collapsed)
      grid-propagated-w-alt-propagator (alt-propagate loc grid-w-loc-collapsed)]
  (do 
    (clojure.pprint/pprint grid-propagated-w-propagator)
    (clojure.pprint/pprint grid-propagated-w-alt-propagator)
    (println (= grid-propagated-w-propagator grid-propagated-w-alt-propagator))
    ))

(clojure.pprint/pprint
 (propagate [1 1] (swap-possible-tiles #{:s} [1 1]  (make-grid 3 3 #{:s :l :c}))))
     

(defn grid-iterator [grid]
  (let [[mel _] (min-entropy-loc grid)
        new-grid (collapse-loc mel grid)]
    (if (fully-collapsed? new-grid)
      new-grid
      (alt-propagate mel new-grid))))

(defn iterate-grid [grid]
  (iterate grid-iterator grid))

(defn run [grid]
  (some (fn [g]
          (when (fully-collapsed? g)
            g))
        (iterate-grid grid)))

(print-grid
 (run (make-grid 10 10  #{:s :l :c}))
10)
