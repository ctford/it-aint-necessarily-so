(ns klangmeister.actions)

(defrecord Play [target])
(defrecord PlayOnce [target])
(defrecord Doc [string target])
(defrecord Test [target])
(defrecord Refresh [text target])
