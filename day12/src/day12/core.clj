(ns day12.core
  (:gen-class))

(defn make-moon [x y z]
  {:x {:pos x :vel 0} :y {:pos y :vel 0} :z {:pos z :vel 0}})

(defn line-to-moon [line]
  (as-> line <>
    (re-find #"<x=([-\d]+), y=([-\d]+), z=([-\d]+)>" <>)
    (let [[_ x y z] <>]
      (make-moon (Integer/parseInt x)
                 (Integer/parseInt y)
                 (Integer/parseInt z)))))

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (map line-to-moon)))

(defn apply-gravity [moon-axis other-moon-axis]
  (let [pos (:pos moon-axis)
        other-pos (:pos other-moon-axis)]
    (update moon-axis :vel
            (cond (> pos other-pos) dec
                  (< pos other-pos) inc
                  :else identity))))

(defn apply-gravities [moon-axis other-moon-axes]
  (reduce apply-gravity moon-axis other-moon-axes))

(defn simulate-gravity [moon-axes]
  (loop [moon-axes moon-axes
         n (count moon-axes)]
    (if (> n 0)
      (let [rest-moon-axes (rest moon-axes)]
        (recur (conj (vec rest-moon-axes)
                     (apply-gravities (first moon-axes) rest-moon-axes))
               (dec n)))
      moon-axes)))

(defn apply-velocity [moon-axis]
  (update moon-axis :pos (fn [pos] (+ pos (:vel moon-axis)))))

(defn step [moon-axes]
  (->> moon-axes
       simulate-gravity
       (map apply-velocity)))

(defn run-simulation [moons num-of-steps]
  (loop [xs (map :x moons)
         ys (map :y moons)
         zs (map :z moons)
         n num-of-steps]
    (if (> n 0)
      (recur (step xs) (step ys) (step zs) (dec n))
      (map #(assoc {} :x %1 :y %2 :z %3) xs ys zs))))

(defn get-potential-energy [{{x :pos} :x {y :pos} :y {z :pos} :z}]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn get-kinetic-energy [{{x :vel} :x {y :vel} :y {z :vel} :z}]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn find-steps-until-repeated-state [moons]
  (let [f (fn [moon-axes]
            (loop [moon-axes moon-axes
                   initial-state nil
                   num-of-steps 0]
              (if (= moon-axes initial-state)
                num-of-steps
                (recur (step moon-axes)
                       (or initial-state moon-axes)
                       (inc num-of-steps)))))
        xs (map :x moons)
        ys (map :y moons)
        zs (map :z moons)]
    (reduce lcm (pmap f [xs ys zs]))))

(defn -main [& args]
  (let [moons (parse-input (slurp "input.txt"))]
    (println
     "Part 1:"
     (as-> moons <>
       (run-simulation <> 1000)
       (map #(* (get-potential-energy %) (get-kinetic-energy %)) <>)
       (reduce + <>)))
    (println "Part 2:" (find-steps-until-repeated-state moons))))
