(ns wfclj.core
  (:require [clojure.set]
            [clojure.pprint]
            [clansi :refer :all]))

;;shared logic
(defn valid-dirs [[x y] [max-x max-y]]
  (cond-> []
    (> x 0) (conj [-1 0])
    (< x (dec max-x)) (conj [1 0])
    (> y 0) (conj [0 -1])
    (< y (dec max-y)) (conj [0 1])))

;; derive compatibillities and weights
(defn dimensions [matrix]
  [(count matrix)
   (count (first matrix))])

(defn derive-weights [matrix]
  (frequencies
   (apply concat matrix)))

(defn rules [matrix dims]
  (into #{}
        (apply concat
               (for [[x row] (map-indexed vector matrix)
                     [y tile] (map-indexed vector row)]
                 (for [[dx dy :as d] (valid-dirs [x y] dims)]
                   (let [other-tile (get-in matrix [(+ x dx) (+ y dy)])]
                     [tile d other-tile]))))))

(defn derive-compatibillities [matrix]
  (let [dims (dimensions matrix)
        flat-rules (rules matrix dims)]
    (reduce (fn [rule-map [tile d other-tile]]
              (update-in rule-map [tile d] (fnil conj #{other-tile}) other-tile))
            {}
            flat-rules)))

;; main logic
(defn make-grid [x-dim y-dim tiles]
  (with-meta
    (into (sorted-map)
          (for [x (range x-dim)
                y (range y-dim)]
            [[x y] [tiles (valid-dirs [x y] [x-dim y-dim])]]))
    {:cols x-dim}))

(defn colorize [kw]
  (get {:l (style "l" :bg-green)
        :c (style "c" :bg-yellow)
        :s (style "s" :bg-blue)
        :A (style "A" :bg-green)
        :B (style "B" :bg-black)
        :C (style "C" :bg-yellow)}
       kw))

(defn print-grid [grid]
  (let [cols (:cols (meta grid))]
    (println
     (clojure.string/join "\n"
     (for [row (partition cols grid)]
       (apply str
              (map (fn [[_ [l _]]]
                     (colorize (first l))) row)))))))

(defn collapsed-val [loc grid]
  (ffirst (get grid loc)))

(defn neighbour [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn collapsed? [loc grid]
  (= 1 (count (first (get grid loc)))))

(defn fully-collapsed? [grid]
  (when
      (every? true? (map collapsed? (keys grid) (repeat grid)))
    grid))

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
                (select-keys (:weights (meta grid)) (seq (tiles-loc loc grid))))]
    (swap-possible-tiles #{choice} loc grid)))

(defn shannon-entropy [loc grid]
  (let [weights (vals (select-keys (:weights (meta grid)) (seq (tiles-loc loc grid))))
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

(defn check-compatibility [tile other-tile dir grid]
  (let [comps (get-in (:comps (meta grid)) [tile dir])]
    (contains? comps other-tile)))

(defn check-if-other-tile-compatible-with-current-tiles-in-direction [[grid stack] [tiles other-tile direction neighbour]]
  (let [other-tile-is-possible
        (some true? (map check-compatibility tiles (repeat other-tile) (repeat direction) (repeat grid)))]
    (if (not other-tile-is-possible)
      [(remove-from-possible-tiles other-tile neighbour grid)
       (conj stack neighbour)]
      [grid stack])))

(defn get-other-tiles [grid loc tiles]
  (let [directions (second (get grid loc)) ;; we get the precomputed valid-dirs
        neighbours (map #(neighbour loc %) directions)]
    (for [[direction neighbour] (partition 2 (interleave directions neighbours))
          other-tile (tiles-loc neighbour grid)]
      [tiles other-tile direction neighbour])))

(defn propagate [loc grid]
  (loop [grid grid
         stack [loc]]
    (if (empty? stack)
      grid
      (let [loc (peek stack)
            stack (pop stack)
            tiles (tiles-loc loc grid)
            other-tiles (get-other-tiles grid loc tiles)
            [grid stack] (reduce check-if-other-tile-compatible-with-current-tiles-in-direction [grid stack] other-tiles)]
        (recur grid stack)))))

(defn grid-iterator [grid]
  (let [[mel _] (min-entropy-loc grid)
        new-grid (collapse-loc mel grid)]
    (if (fully-collapsed? new-grid)
      new-grid
      (propagate mel new-grid))))

(defn iterate-grid [grid]
  (iterate grid-iterator grid))

(defn run [grid]
  (some fully-collapsed? (iterate-grid grid)))

(defn derive-and-run [grid matrix]
  (let [compatibillities (derive-compatibillities matrix)
        weights (derive-weights matrix)]
    (run
      (vary-meta grid merge {:comps compatibillities :weights weights}))))

(defn make-grid-and-run [x-dim y-dim input-matrix]
  (let [compatibillities (derive-compatibillities input-matrix)
        weights (derive-weights input-matrix)
        tiles (set (keys weights))]
    (run
      (vary-meta (make-grid x-dim y-dim tiles) merge {:comps compatibillities :weights weights}))))

(def input-matrix-1
  [[:l :l :l :l]
   [:l :l :l :l]
   [:l :l :l :l]
   [:l :c :c :l]
   [:c :s :s :c]
   [:s :s :s :s]
   [:s :s :s :s]])

(def input-matrix-2
  [[:A :A :A :A]
   [:A :A :A :A]
   [:A :A :A :A]
   [:A :C :C :A]
   [:C :B :B :C]
   [:C :B :B :C]
   [:C :B :B :C]])

(print-grid
 (make-grid-and-run 20 20 input-matrix-1))

(print-grid
 (derive-and-run (make-grid 20 20 #{:l :c :s}) input-matrix-1)
 ;;(derive-and-run (make-grid 20 20  #{:A :B :C}) input-matrix-2)
 )
