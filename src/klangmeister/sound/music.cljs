(ns klangmeister.sound.music
  (:require [cljs-bach.synthesis :as synthesis]))

(defn play-from!
  "Take a sequence of notes and play them from a set point in an audiocontext."
  [audiocontext from notes]
  (doseq [{:keys [time instrument] :as note} notes]
    (let [at (+ time from)
          duration 5.2
          synth-instance (-> note
                             (dissoc :time)
                             instrument)
          connected-instance (synthesis/connect synth-instance synthesis/destination)]
      (connected-instance audiocontext at duration))))

(defn play!
  "Take a sequence of notes and play them in an audiocontext."
  [audiocontext notes]
  (play-from! audiocontext (synthesis/current-time audiocontext) notes))
