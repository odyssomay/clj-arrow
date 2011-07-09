(ns clj-arrow.arrow)

(let [multithread (atom false)]
  (defn set-multithreading-on [& _]
    (reset! multithread true))

  (defn set-multithreading-off [& _]
    (reset! multithread false))

  (defn is-multithreading? [& _]
    @multithread))

(defn clone [x]
  [x x])

(defn swap [[x y]]
  [y x])

(defn arr [f]
  f)

(defmacro flow [& flows]
  `(-> ~(first flows) ~@(partition 2 (rest flows))))

(defprotocol Arrow
  (run [this value])
  (>>>_int [this dest])
  (fst_int [this]))

(extend-type clojure.lang.IFn
  Arrow
  (run [this value] (this value))
  (>>>_int [this dest]
       #(run dest (this %)))
  (fst_int [this]
           (fn [[x y]]
             [(this x) y])))

(defn >>> 
  "Create an arrow where the first
  arrow is applied to the input, the
  second arrow is applied to the result, 
  the third arrow is applied to the 
  second result, and so on...

Example:
  ((>>> inc inc inc) 0)
=> 3"
  [& arrs]
  (case (count arrs)
    1 (first arrs)
    2 (>>>_int (first arrs) (second arrs))
    (>>>_int (first arrs) (apply >>> (rest arrs)))))

(defn <<< 
  "The reverse of >>>"
  [& arrs]
  (apply >>> (reverse arrs)))

(defn fst 
  "Create an arrow that takes a pair
  and applies arr on the first value.

Example:
  ((fst inc) [1 1])
=> [2 1]"
  [arr]
  (fst_int arr))

(defn snd 
  "As fst, but applies arr on the 
  second value.

Example:
  ((fst inc) [1 1])
=> [1 2]"
  [arr]
  (>>> swap (fst arr) swap))

(defn *** 
  "Create an arrow that takes a pair,
  applies arr1 on the first value
  and applies arr2 on the second value.

Example:
  ((*** inc dec) [1 1])
=> [2 0]"
  [arr1 arr2]
  (if (is-multithreading?)
    (fn [[x y]] 
      (vec (pvalues (run arr1 x) (run arr2 y))))
    (>>> (fst arr1) (snd arr2))))

(defn ***-multithread
  "see ***"
  [arr1 arr2]
  (fn [[x y]]
    (vec (pvalues (run arr1 x) (run arr2 y)))))

(defn &&& 
  "As *** but takes a single value
  and clones it.

Example:
  ((&&& inc dec) 1)
=> [2 0]"
  [arr1 arr2]
  (>>> clone (*** arr1 arr2)))

(defn &&&-multithread
  "same as &&&"
  [arr1 arr2]
  (>>> clone (***-multithread arr1 arr2)))

(defn |||
  "Create an arrow that takes a pair.
  arr1 will be applied to the first value,
  if the result is truthy the pair with the
  resulting value is sent to arr2, if falsey 
  it is sent to arr3.

  The two arrow version has arr3 as an arrow
  that always returns nil.

Example:
  ((||| nil? first second) [0 1])
=> 0
  ((||| nil? first second) [nil 1])
=> 1"
  ([arr1 arr2]
   (||| arr1 arr2 (constantly nil)))
  ([arr1 arr2 arr3]
   (>>> (fst arr1) #(if (first %) (arr2 %) (arr3 %)))))

