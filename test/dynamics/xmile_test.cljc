(ns dynamics.xmile-test
  (:require [clojure.test :refer [deftest is testing]]
            [dynamics.xmile :as dx]
            [xmile.model :as m]
            [xmile.validate :as validate]
            [xmile.execute :as execute]))

(def xmile-ns
  {:model m/model :sim-specs m/sim-specs :aux m/aux :flow m/flow
   :stock m/stock :add-variable m/add-variable})

(deftest acquisition-model-builds-a-valid-real-xmile-model-test
  (testing "the generic acquisition-model, given real params, is a valid XMILE model per xmile.validate"
    (let [built (dx/acquisition-model xmile-ns {:name "test-acq" :inflow-rate 100 :conversion-rate 0.01
                                                 :initial-stock 5 :sim-days 10})]
      (is (validate/valid? (validate/validate built))))))

(deftest acquisition-model-projects-correct-linear-growth-test
  (testing "with a constant inflow*rate and no feedback, the stock grows exactly linearly -- verify against the closed-form answer, not just 'it runs'"
    (let [built (dx/acquisition-model xmile-ns {:name "linear-check" :inflow-rate 200 :conversion-rate 0.05
                                                 :initial-stock 10 :sim-days 20} {:dt 1.0 :method :rk4})
          projected (dx/project execute/run built [0 10 20])
          ;; closed form: stock(t) = initial + inflow*rate*t = 10 + 200*0.05*t = 10 + 10t
          expected {0 10.0 10 110.0 20 210.0}]
      (doseq [[day exp] expected]
        (let [diff (- exp (get-in projected [:checkpoints day]))]
          (is (< (if (neg? diff) (- diff) diff) 1e-6)
              (str "day " day " expected " exp " got " (get-in projected [:checkpoints day]))))))))

(deftest project-omits-checkpoints-past-sim-stop-test
  (testing "a checkpoint day beyond :xmile/stop is silently omitted, not extrapolated"
    (let [built (dx/acquisition-model xmile-ns {:name "short-horizon" :inflow-rate 10 :conversion-rate 0.1
                                                 :initial-stock 0 :sim-days 5})
          projected (dx/project execute/run built [0 5 100])]
      (is (contains? (:checkpoints projected) 0))
      (is (contains? (:checkpoints projected) 5))
      (is (not (contains? (:checkpoints projected) 100))))))

(deftest etzhayyim-f2-real-projection-test
  (testing "the actual etzhayyim F2-upper-bound projection this cycle computed, pinned as a regression check against the real observed numbers (5th observation, 2026-07-21): avg 1846.4 weekly uniques, F2 upper bound 0.00017822024693214808"
    (let [built (dx/acquisition-model xmile-ns
                                       {:name "etzhayyim-adherent-acquisition"
                                        :inflow-rate (/ 1846.4 7.0)
                                        :conversion-rate 0.00017822024693214808
                                        :initial-stock 1
                                        :sim-days 3650})
          projected (dx/project execute/run built [365 1825 3650])]
      ;; even under the MOST OPTIMISTIC rate consistent with zero observed conversions,
      ;; projected adherents after 10 years is still under 200
      (is (< 15 (get-in projected [:checkpoints 365]) 20))
      (is (< 80 (get-in projected [:checkpoints 1825]) 95))
      (is (< 165 (get-in projected [:checkpoints 3650]) 180)))))
