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

(defn constrain [tiles1 tiles2]
  (clojure.set/intersection tiles1 tiles2))

(defn tiles-loc [loc grid]
  (first (get grid loc)))  

(defn swap-possible-tiles [tiles loc grid]
  (assoc-in grid [loc 0] tiles))

(defn constrain-dir [loc dir grid]
  (let [val (collapsed-val loc grid)
        loc-neighbour (neighbour loc dir)
        possible-tiles (tiles-loc loc-neighbour)
        allowed-tiles (get-in compabilities [val dir])
        constrained-tiles (constrain possible-tiles allowed-tiles)]
    (swap-possible-tiles constrained-tiles loc-neighbour grid)))

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
  (let [comps (get-in compabilities [tile dir]) 
        res (contains? comps other-tile)]
  ;;(println "checking if" other-tile " is compatible with " comps " for " tile " in direction " dir " result: " res)
  res))

(defn propagate [loc grid]
  (let [stack (atom [loc])
        grid (atom grid)]
    (while (not-empty @stack)
      (println "Stack upon enter: " @stack)
      (let [[s l] [(pop @stack) (peek @stack)]
            cur-possible-tiles (tiles-loc l @grid)]
        (reset! stack s)
        (doall
         (for [direction (second (get @grid l))];; we get the precomputed valid-dirs
           (let [n (neighbour l direction)
                 ;;dummy (println "Considering " (second (get @grid l)) " directions, for location " l)
                 ]
             (doall
              (for [other-tile (tiles-loc n @grid)]
                (let [other-tile-is-possible
                      (some true? (map check-compatibility
                                       cur-possible-tiles
                                       (repeat other-tile)
                                       (repeat direction)))]
                  (when (not other-tile-is-possible)
                    (reset! grid (remove-from-possible-tiles other-tile n @grid))
                    ;;(println "Removed: " other-tile " from " n " because " other-tile " is not compatible with any of " cur-possible-tiles "in current cell " l  " in direction " direction )
                    ;;(when (= l n) (println "ERROR: " l " 0 " n))
                    ;;(println "New contents of neighbour: " n " is " (get @grid n))
                    (println "Adding: " n " to the stack.")
                    (swap! stack conj n)
                    (println "New stack: " @stack))))))))))
    ;;(clojure.pprint/pprint @grid)
    @grid))


(defn grid-iterator [grid]
  ;; TODO implement iterator
  (let [[mel _] (min-entropy-loc grid)
        new-grid (collapse-loc mel grid)]
    (if (fully-collapsed? new-grid)
      new-grid
      (propagate mel new-grid))))

(defn iterate-grid [grid]
  (iterate grid-iterator grid))

(defn run [grid]
  (some (fn [g]
          (when (fully-collapsed? g)
            g))
        (iterate-grid grid)))
(let [[cols rows] [3 3]
      grid (make-grid cols rows  #{:s :l :c})]
  (map clojure.pprint/pprint
       (run grid)))


(pop [:a :b :c])

(print-grid
(run (make-grid 10 10  #{:s :l :c}))
10)

(clojure.pprint/pprint
 (propagate [1 1] (swap-possible-tiles #{:s} [1 1]  (make-grid 3 3 #{:s :l :c}))))
     

(map #(clojure.pprint/pprint (sort %))
     (take 3 (iterate-grid test-grid)))


(def test-grid
  {[0 0] [#{:s} [[1 0] [0 1]]],
   [0 1] [#{:s :c} [[1 0] [0 -1] [0 1]]],
   [0 2] [#{:s :l :c} [[1 0] [0 -1]]],
   [1 0] [#{:s} [[-1 0] [1 0] [0 1]]],
   [1 1] [#{:s :l :c} [[-1 0] [1 0] [0 -1] [0 1]]]
   [1 2] [#{:s :l :c} [[-1 0] [1 0] [0 -1]]],
   [2 0] [#{:s :l :c} [[-1 0] [0 1]]],
   [2 1] [#{:s :l :c} [[-1 0] [0 -1] [0 1]]],
   [2 2] [#{:s :l :c} [[-1 0] [0 -1]]]})

test-grid


(check-compatibility :c :s [1 0])

(clojure.pprint/pprint compabilities)
(clojure.pprint/pprint
 (sort
  (flat-compabilities input-matrix-1)))

(defn alt-print-grid [grid cols]
  (clojure.pprint/pprint
   (for [row (partition cols grid)]
     (map (fn [[_ [l _]]] l) row))))

(defn alt-rules [matrix dims]
  (into
   #{}
   (apply concat
          (for [[x row] (map-indexed vector matrix)
                [y tile] (map-indexed vector row)]
            (for [[dx dy :as d] (valid-dirs [x y] dims)]
              (let [other-tile (get-in matrix [(+ x dx) (+ y dy)])]
                [tile other-tile d]))))))

(defn flat-compabilities [matrix]
  (let [dims (dimensions matrix)]
         (alt-rules matrix dims)))

(defn check-compatibility [tile other-tile dir]
  (contains? 
   (get-in compabilities [tile dir])
   other-tile))

(defn update-values [m f & args]
  (into {} (for [[k v] m] [k (apply f v args)])))

