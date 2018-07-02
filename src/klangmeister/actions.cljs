(ns klangmeister.actions)

(defrecord PlayOnce [target])
(defrecord Doc [string target])
(defrecord Test [target])
(defrecord Refresh [text target])
