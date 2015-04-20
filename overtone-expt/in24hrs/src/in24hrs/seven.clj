(ns in24hrs.seven
  (use [in24hrs.core]
       [overtone.live]
       [overtone.inst.sampled-piano]))

(def anchor (atom :e2))
(def anchor-list [:b3 :b3 :d4 :b3 :a3 :g3 :f#3])
(def dur-list [1/4 1/2 3/4 1 5/4 3/2 7/4 2])

(defn play-riff
  "doc-string"
  [beat comp]
  (let [note-list (first comp)
        a-note (first note-list)
        duration (second note-list)
        gap (second (rest note-list))]
    (play sampled-piano beat (note a-note) 1 duration)
    (apply-at (metro (+ beat duration gap)) play-riff (+ beat duration gap) (concat (rest comp) [(first comp)]) [])))

(defn play-chord
  [notes-list]
  (let [chord-notes (first notes-list)
        duration (second notes-list)
        ]))

(defn play-phrase
  "doc-string"
  [beat chord-matrix]
  (let [chord-notes (first chord-matrix)
        duration (second chord-notes)
        gap (second (rest chord-notes))]
     (reset! anchor (rand-nth anchor-list))
    (doseq [notes (first chord-notes)]
      (play sampled-piano beat (note notes) 1 duration))
    (apply-at (metro (+ beat duration gap)) play-phrase (+ beat duration gap) (concat (rest chord-matrix) [chord-notes]) [])))


(defn improv-solo [beat note-list]
  (play sampled-piano beat
       (find-closest-elem (sinr beat (sinr beat 4 0 5/7) (+ 12 (note @anchor)) 5/7) (map #(+ 12 (note %)) note-list))
       1.5
       (rand-nth dur-list))
  (apply-at (metro (+ 1/2 beat)) improv-solo (+ 1/2 beat) note-list []))


(play-riff (* 4 (metro-bar metro)) [[:e2 3/2 0]
                                    [:e2 1/4 1/4]
                                    [:g2 1/4 1/2]
                                    [:e2 1/4 1/2]
                                    [:d2 1/2 0]
                                    [:c2 2 0]
                                    [:b1 2 0]])

(drum-loop (* 4 (metro-bar metro)))

(play-phrase (* 4 (metro-bar metro)) [[[:e4 :b3 :e3] 3/2 0]
                                      [[:e4 :b3 :e3] 1/2 0]
                                      [[:g4 :d4 :g3] 1/2 1/4]
                                      [[:e4 :b3 :e3] 1/2 1/4]
                                      [[:d4 :a3 :d3] 1/2 0]
                                      [[:c4 :g3 :c3] 2 0]
                                      [[:b3 :f#3 :b2] 2 0]
                                      [[:e4 :b3 :e3] 3/2 0]
                                      [[:e4 :b3 :e3] 1/2 0]
                                      [[:g4 :d4 :g3] 1/2 1/4]
                                      [[:e4 :b3 :e3] 1/2 1/4]
                                      [[:d4 :a3 :d3] 1/2 0]
                                      [[:c4 :g3 :c3] 3/4 0]
                                      [[:d4 :a4 :d3] 3/4 0]
                                      [[:c4 :g3 :c3] 1/2 0]
                                      [[:b3 :f#3 :b2] 2 0]])

(improv-solo (* 4 (metro-bar metro)) anchor-list)

(stop)
