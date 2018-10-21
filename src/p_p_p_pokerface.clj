(ns p-p-p-pokerface)

(def replacement {\T 10, 
                  \J 11,
                  \Q 12,
                  \K 13,
                  \A 14})

; Types of hands in poker
;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn rank [card]
  (let [[fst _] card]             ; fst is a character like \C
    (if (Character/isDigit fst)   ; If fst is digit
      (Integer/valueOf (str fst)) ;   Then return integer of digit
      (replacement fst))))        ;   Else return integer of Leter

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (if (.contains freqs 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (if (.contains freqs 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (if (.contains freqs 4)
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (if (.contains freqs 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (if (and (.contains freqs 3) (.contains freqs 2))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        freq-of-freqs (vals (frequencies freqs))]
    (if (or (.contains freqs 4)
            (and (.contains freq-of-freqs 2)
                 (.contains freqs 2)))
      true
      false)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted (sort ranks)
        [r1 r2 r3 r4 r5] sorted
        alt-ranks (replace {14 1} ranks)
        alt-sorted (sort alt-ranks)
        [a1 a2 a3 a4 a5] alt-sorted]
    (if (or (and (< r1 r2 r3 r4 r5)
                 (= (- r5 r1) 4))
            (and (< a1 a2 a3 a4 a5)
                 (= (- a5 a1) 4)))
      true
      false)))
        

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    true
    false))

(defn value [hand]
  (if (straight-flush? hand)
    8
    (if (four-of-a-kind? hand)
      7
      (if (full-house? hand)
        6
        (if (flush? hand)
          5
          (if (straight? hand)
            4
            (if (three-of-a-kind? hand)
              3
              (if (two-pairs? hand)
                2
                (if (pair? hand)
                  1
                  0)))))))))
