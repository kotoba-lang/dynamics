(ns dynamics.xmile-test
  (:require [clojure.test :refer [deftest is testing]]
            [dynamics.xmile :as dx]
            [xmile.model :as m]
            [xmile.validate :as validate]
            [xmile.execute :as execute]))

(def xmile-ns
  {:model m/model :sim-specs m/sim-specs :aux m/aux :flow m/flow
   :stock m/stock :add-variable m/add-variable})

(defn- exp [x]
  #?(:cljs (js/Math.exp x)
     :clj (Math/exp x)))

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

(deftest percentage-rate-model-builds-a-valid-real-xmile-model-test
  (testing "the generic percentage-rate-model, given a real rate, is a valid XMILE model"
    (let [built (dx/percentage-rate-model xmile-ns {:name "test-rate" :initial-stock 100 :annual-rate 0.1 :sim-years 5})]
      (is (validate/valid? (validate/validate built))))))

(deftest percentage-rate-model-matches-continuous-exponential-closed-form-test
  (testing "Stock' = Stock * rate integrates to the closed-form continuous exponential S0*e^(rt), not discrete-compound S0*(1+r)^t -- verify against the correct closed form"
    (let [built (dx/percentage-rate-model xmile-ns {:name "exp-check" :initial-stock 1000 :annual-rate 0.2 :sim-years 10} {:dt 0.01 :method :rk4})
          projected (dx/project execute/run built [5 10])
          expected-5 (* 1000 (exp (* 0.2 5)))
          expected-10 (* 1000 (exp (* 0.2 10)))]
      (is (< 0.99 (/ (get-in projected [:checkpoints 5]) expected-5) 1.01))
      (is (< 0.99 (/ (get-in projected [:checkpoints 10]) expected-10) 1.01)))))

(deftest percentage-rate-model-supports-negative-decline-rates-test
  (testing "a negative annual-rate produces real decay, not growth, and stays positive (never crosses zero for a proportional model)"
    (let [built (dx/percentage-rate-model xmile-ns {:name "decline-check" :initial-stock 100 :annual-rate -0.1 :sim-years 20})
          projected (dx/project execute/run built [1 10 20])]
      (is (> (get-in projected [:checkpoints 1]) (get-in projected [:checkpoints 10]) (get-in projected [:checkpoints 20])))
      (is (pos? (get-in projected [:checkpoints 20]))))))

(deftest aca-marketplace-real-decline-projection-test
  (testing "the actual ACA marketplace decline projection this cycle computed, pinned as a regression check: real 2025->2026 enrollment (24.3M -> 23.1M, -4.938% annual rate)"
    (let [rate (- (/ 23.1 24.3) 1)
          built (dx/percentage-rate-model xmile-ns {:name "aca-decline" :initial-stock 23.1 :annual-rate rate :sim-years 30})
          projected (dx/project execute/run built [10])
          crossing (dx/crossing-year execute/run built rate 12.15)]
      (is (< -0.0494 rate -0.0493))
      (is (< 13.9 (get-in projected [:checkpoints 10]) 14.3))
      (is (< 12.5 crossing 13.5)))))

(defn- bass-closed-form [p q M t]
  (let [e (exp (* (- (+ p q)) t))
        f (/ (- 1 e) (+ 1 (* (/ q p) e)))]
    (* M f)))

(deftest bass-diffusion-model-builds-a-valid-real-xmile-model-test
  (testing "the generic bass-diffusion-model, given real params, is a valid XMILE model"
    (let [built (dx/bass-diffusion-model xmile-ns {:name "test-bass" :market-size 1000 :p-coefficient 0.03
                                                     :q-coefficient 0.38 :initial-adopters 0 :sim-time 10})]
      (is (validate/valid? (validate/validate built))))))

(deftest bass-diffusion-model-matches-closed-form-test
  (testing "matches Bass's own closed-form solution A(t) = M * (1-e^-(p+q)t) / (1+(q/p)e^-(p+q)t) to high precision -- not just 'it runs', verified against the textbook analytic answer"
    (let [p 0.03 q 0.38 M 1000
          built (dx/bass-diffusion-model xmile-ns {:name "bass-closed-form-check" :market-size M :p-coefficient p
                                                     :q-coefficient q :initial-adopters 0 :sim-time 20} {:dt 0.02})
          projected (dx/project execute/run built [1 5 10 15 20])]
      (doseq [t [1 5 10 15 20]]
        (let [expected (bass-closed-form p q M t)
              actual (get-in projected [:checkpoints t])]
          (is (< 0.999 (/ actual expected) 1.001)
              (str "t=" t " expected " expected " got " actual)))))))

(deftest bass-diffusion-model-produces-an-s-curve-with-nonzero-q-test
  (testing "a nonzero q (imitation/internal influence) produces acceleration through the middle -- the growth rate in the middle window must exceed the growth rate in the first window, unlike a pure p-only (no-feedback) model which only decelerates"
    (let [built (dx/bass-diffusion-model xmile-ns {:name "s-curve-check" :market-size 1000 :p-coefficient 0.01
                                                     :q-coefficient 0.4 :initial-adopters 0 :sim-time 15} {:dt 0.02})
          projected (dx/project execute/run built [0 2 6 8])
          early-rate (/ (- (get-in projected [:checkpoints 2]) (get-in projected [:checkpoints 0])) 2)
          mid-rate (/ (- (get-in projected [:checkpoints 8]) (get-in projected [:checkpoints 6])) 2)]
      (is (> mid-rate early-rate)))))

(deftest bass-diffusion-model-p-only-decelerates-immediately-like-acquisition-model-test
  (testing "with q=0 (no imitation channel), the model degenerates to pure decelerating adoption from t=0 -- no S-curve acceleration, matching the structural claim in the docstring"
    (let [built (dx/bass-diffusion-model xmile-ns {:name "p-only-check" :market-size 1000 :p-coefficient 0.05
                                                     :q-coefficient 0.0 :initial-adopters 0 :sim-time 10} {:dt 0.02})
          projected (dx/project execute/run built [0 1 2 3])
          rate-1 (- (get-in projected [:checkpoints 1]) (get-in projected [:checkpoints 0]))
          rate-2 (- (get-in projected [:checkpoints 2]) (get-in projected [:checkpoints 1]))
          rate-3 (- (get-in projected [:checkpoints 3]) (get-in projected [:checkpoints 2]))]
      (is (> rate-1 rate-2 rate-3)))))
