(ns run-tests
  (:require [cljs.test :as t]
            [dynamics.core-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (when-not (t/successful? m)
    (js/process.exit 1)))

(t/run-tests 'dynamics.core-test)
