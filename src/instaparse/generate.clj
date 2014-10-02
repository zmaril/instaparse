(ns instaparse.generate
  (:use instaparse.core)
  (:require [clojure.string :as s]))

(ns-unmap 'instaparse.generate 'generate*)
;http://stackoverflow.com/questions/18246549/cartesian-product-in-clojure
(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defmulti  generate* (fn [_ {:keys [tag] :as m}]
                       (println m) tag))

(defmethod generate* :string
  [_ {:keys [string]}]
  [string])

(defmethod generate* :opt
  [grammar p]
  (cons "" (generate* grammar (:parser p))))

(defmethod generate* :nt
  [grammar {:keys [keyword]}]
  (generate* grammar (grammar keyword)))

(defmethod generate* :cat
  [grammar {:keys [parsers]}]
  (->> parsers
       (map (partial generate* grammar) )
       cart
       (map s/join)))

(defmethod generate* :alt
  [grammar {:keys [parsers]}]
  (apply concat (map (partial generate* grammar) parsers)))

(defmethod generate* :star
  [grammar p]
  (let [result (generate* grammar (:parser p))]
    (->> [""]
         (iterate
          (fn [r] (map s/join (cart (vector result r)))))
         flatten)))

(defmethod generate* :plus
  [grammar p]
  (let [result (generate* grammar (:parser p))]
    (->> result
         (iterate
          (fn [r] (map s/join (cart (vector result r)))))
         flatten)))

(defmethod generate* :neg
    [grammar {:keys [parsers]}]
    (let [[neg p] parsers
          results  (generate* grammar p)
          negative-grammar
          (-> grammar
              (assoc :start-production :NEG-PLACEHOLDER!)
              (assoc-in [:grammar :NEG-PLACEHOLDER!] (:parser neg)))]
      (filter (comp not string? negative-grammar) results)))

(defn generate
  [{:keys [start-production grammar]}]
  (generate* grammar (grammar start-production)))

(->> "s = 'a' 'a' | 'b' | 'c' | d | e | f
     d = 'd'
     e = 'e' 'e'
     f = 'f' +"
    parser
    generate
    (take 20))

(->> "s = ('a' | 'b')*"
    parser
    generate
    (take 10)
    )


(def negative-lookahead "S = &'ab' ('a' | 'b')+")
(-> (parser negative-lookahead)
    :grammar)


(->> (parser "S = !'ab' ('a' | 'b')+")
     ;generate
     ;(take 10)
     )

(->> "S = AB*
      AB = A B
      A = 'a'+
      B = 'b'+"
     parser
     generate
     (take 10)
 )

(def myname
  (parser
   "myname = first <' '> middle <' '> last
    <first> = 'Zack' | 'Zach' | 'Zachary'
    middle = 'Will' 'iam'
    last = 'Merril' | 'Maril'"))

(generate myname)
