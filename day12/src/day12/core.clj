(ns day12.core
  (:gen-class))

(defn make-moon [x y z]
  {:pos {:x x :y y :z z} :vel {:x 0 :y 0 :z 0}})

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

(defn apply-gravity [moon other-moon]
  (reduce #(let [pos (get-in moon [:pos %2])
                 other-pos (get-in other-moon [:pos %2])]
             (update-in %1 [:vel %2]
                        (cond (> pos other-pos) dec
                              (< pos other-pos) inc
                              :else identity)))
          moon
          (keys (:pos moon))))

(defn apply-gravities [moon other-moons]
  (reduce apply-gravity moon other-moons))

(defn simulate-gravity [moons]
  (loop [moons moons
         n (count moons)]
    (if (> n 0)
      (let [rest-moons (rest moons)]
        (recur (conj (vec rest-moons)
                     (apply-gravities (first moons) rest-moons))
               (dec n)))
      moons)))

(defn apply-velocity [moon]
  (reduce #(let [vel (get-in moon [:vel %2])]
             (update-in %1 [:pos %2] (fn [pos] (+ pos vel))))
          moon
          (keys (:pos moon))))

(defn step [moons]
  (->> moons
      simulate-gravity
      (map apply-velocity)))

(defn run-simulation [moons num-of-steps]
  (loop [moons moons
         n num-of-steps]
    (if (> n 0)
      (recur (step moons) (dec n))
      moons)))

(defn get-potential-energy [{{:keys [x y z]} :pos}]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn get-kinetic-energy [{{:keys [x y z]} :vel}]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn find-steps-until-repeated-state [moons]
  (loop [moons moons
         previous-states []
         num-of-steps 0]
    (if (some #(= moons %) previous-states)
      num-of-steps
      (let [new-moons (step moons)]
        (recur new-moons (conj previous-states moons) (inc num-of-steps))))))

(defn -main [& args]
  (let [moons (parse-input (slurp "input.txt"))]
    (println
     "Part 1:"
     (as-> moons <>
       (run-simulation <> 1000)
       (map #(* (get-potential-energy %) (get-kinetic-energy %)) <>)
       (reduce + <>)))
    (println "Part 2:" (find-steps-until-repeated-state moons))))
