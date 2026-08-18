(ns dynamics.kotoba-score-core-parity-test
  "Parity gate between `dynamics.core` -- the authority, unchanged -- and its
  Kotoba decision core, `kotoba/dynamics_score_core.kotoba`.

  The port is compiled here and executed through the KIR interpreter in this
  same JVM, so no value crosses a runtime boundary: an f64 parameter is an
  ordinary Clojure double going in and coming back out.

  WHAT IS COMPARED. Not `Math/pow` against `f64-exp-bounded` -- that would be a
  test of two kernels. Every case below calls the REAL public function of
  `dynamics.core` and asserts the port reproduces the number it returns.

  EXACT vs TOLERANT. The two halves of this gate are deliberately different and
  must not be merged:

    exact    band-weight, leverage-score's :base-score, :expected-yield,
             loop-structural-strength, real-growth. Both sides are IEEE
             add/sub/mul/div or a selection; a difference of one ulp here is a
             defect, not rounding. loop-structural-strength is the load-bearing
             case: `(* a b c d)` folds left, and regrouping those four
             multiplications is algebraically identical and not identical in
             IEEE, so this half is what would catch a regrouping.

    tolerant pow, upper-bound-rate-from-zero-events, cagr. The .cljc calls the
             host math library (`Math/pow` on the JVM, `js/Math.pow` on
             ClojureScript); the port evaluates the fixed exp/log polynomial
             kernels (amu docs/adr/0012), which import no host transcendental.
             Bit equality is therefore not available and claiming it would be a
             lie. The bound asserted is `tolerance`; the max error actually
             observed is printed on every run, so drift toward the bound is
             visible before it crosses.

  REFUSAL. `dynamics.core` says nil -- never 0 -- where a measure is not
  computable, and raises where its own precondition is violated. The port has
  no `throw` (root CLAUDE.md, \":intentional-security-constraint\") and returns
  `[:result :f64 :string]`. The mechanisms differ, so they are compared as
  BOTH REFUSE, not as `both raise the same thing`.

  WHAT THIS GATE DOES NOT CLAIM. It does not claim `dynamics.core` runs without
  a JVM or a JS engine. `rank-interventions`, `meadows-bands`, `loop-archetypes`,
  `compare-archetypes-2d`, `regime-changes` and `money-loop-measures` are all
  still `.cljc` and all still need a host. What is asserted here is narrower and
  is exactly what it says: the scalar arithmetic that decides a score is
  reproduced by a module that imports no host math."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.compiler.core :as compiler]
            [kotoba.kir :as ir]
            [dynamics.core :as d]))

(def ^:private port-source (slurp "kotoba/dynamics_score_core.kotoba"))

(def ^:private kir
  (delay (:kir (compiler/compile-source port-source :wasm32-kotoba-v1 {}))))

(defn- port
  "Call an exported function of the port. Result-returning exports come back as
  [ok? payload]; plain f64 exports come back as a double."
  [f & args]
  (ir/execute @kir f (vec args)))

(defn- ok?    [r] (and (vector? r) (true? (first r))))
(defn- value  [r] (second r))

(defn- cljc-refuses?
  "`dynamics.core` refuses in two ways: nil for `not computable from what we
  have`, and a raised precondition for `you called this wrong`. Both are a
  refusal to hand back a number, which is the property compared here."
  [f & args]
  (try (nil? (apply f args))
       (catch Throwable _ true)))

;; --- the corpus -------------------------------------------------------------
;; Real shapes, not round numbers: cycle times from hours to a year, friction
;; and instrumentation across the whole [0,1] range the docstrings describe,
;; and growth/inflation pairs at both ends of the span `real-growth` exists to
;; handle (2% and 200%).

(def ^:private bands [:band/E :band/D :band/C :band/B :band/A])

(def ^:private tractabilities [0.0 0.05 0.2 0.37 0.5 0.618 0.75 0.9 0.99 1.0])

(def ^:private loop-shapes
  (for [cycle [0.25 1.0 3.5 7.0 14.0 30.0 90.0 365.0 1000.0]
        sf    [0.0 0.15 0.5 0.83 1.0]
        ic    [0.0 0.07 0.5 0.95 1.0]
        fr    [0.0 0.05 0.4 0.9 1.0]]
    {:cycle-time-days cycle :self-funding-coefficient sf
     :instrumentation-completeness ic :friction fr}))

(def ^:private growth-pairs
  (for [nominal   [-0.5 -0.02 0.0 0.021 0.07 0.35 2.0 12.0]
        inflation [-0.99 -0.3 -0.021 0.0 0.02 0.19 2.0 230.0]]
    [nominal inflation]))

(def ^:private cagr-cases
  (for [start [0.5 1.0 97.0 1000.0 8.25e6]
        end   [0.25 1.0 113.0 4200.0 9.9e7]
        years [0.5 1.0 3.0 7.0 25.0]]
    [start end years]))

(def ^:private trial-counts [1 2 3 7 30 100 1000 12345 1000000])
(def ^:private confidences  [0.5 0.9 0.95 0.99 0.999])

(def ^:private tolerance
  "Relative bound for the pow-bearing half. Chosen as a round number well above
  the worst error this corpus actually produces (printed every run), so that a
  real regression crosses it long before rounding noise does."
  1e-12)

(defn- relative-error [expected actual]
  (if (zero? expected)
    (Math/abs (double actual))
    (Math/abs (/ (- (double actual) (double expected)) (double expected)))))

;; --- exact half -------------------------------------------------------------

(deftest band-weight-is-exact
  (testing "every Meadows band reproduces dynamics.core/band-weight exactly"
    (doseq [b bands]
      (let [r (port 'band-weight b)]
        (is (ok? r) (str "port refused a known band: " b))
        (is (= (double (d/band-weight b)) (value r))
            (str "band-weight disagreed for " b)))))
  (testing "an unknown band refuses on both sides"
    (is (nil? (d/band-weight :band/ZZZ)))
    (is (false? (ok? (port 'band-weight :band/ZZZ))))))

(deftest leverage-base-is-exact
  (doseq [b bands t tractabilities]
    (let [expected (:base-score (d/leverage-score {:band b :tractability t}))
          r (port 'leverage-base b t)]
      (is (ok? r) (str "port refused a valid intervention: " b " " t))
      (is (= (double expected) (value r))
          (str "base-score disagreed for " b " tractability " t)))))

(deftest expected-yield-is-exact
  (doseq [pool [1.0 17.0 1250.0 3.3e6 8.1e9]
          rate [0.0 1.0e-5 0.003 0.12 0.5 1.0]]
    (let [expected (:expected-yield
                    (d/leverage-score {:band :band/C :tractability 0.5
                                       :pool-size pool :conversion-rate rate}))]
      (is (= (double expected) (port 'expected-yield pool rate))
          (str "expected-yield disagreed for pool " pool " rate " rate)))))

(deftest loop-structural-strength-is-exact
  (testing "the four-factor product, including its left-associated fold"
    (let [mismatches
          (for [m loop-shapes
                :let [expected (d/loop-structural-strength m)
                      actual (port 'loop-structural-strength
                                   (:cycle-time-days m)
                                   (:self-funding-coefficient m)
                                   (:instrumentation-completeness m)
                                   (:friction m))]
                :when (not= (double expected) actual)]
            [m expected actual])]
      (is (empty? mismatches)
          (str (count mismatches) " of " (count loop-shapes)
               " loop shapes disagreed, first: " (first mismatches))))))

(deftest real-growth-is-exact
  (let [mismatches
        (for [[nominal inflation] growth-pairs
              :let [expected (d/real-growth nominal inflation)
                    r (port 'real-growth nominal inflation)]
              :when (if (nil? expected)
                      (ok? r)
                      (not= (double expected) (value r)))]
          [nominal inflation expected r])]
    (is (empty? mismatches)
        (str (count mismatches) " of " (count growth-pairs)
             " growth pairs disagreed, first: " (first mismatches)))))

;; --- tolerant half ----------------------------------------------------------

(deftest cagr-agrees-within-tolerance
  (let [errors (for [[start end years] cagr-cases
                     :let [expected (d/cagr start end years)
                           r (port 'cagr start end years)]]
                 (do (is (= (nil? expected) (not (ok? r)))
                         (str "refusal disagreed for " [start end years]))
                     (if (and expected (ok? r))
                       (relative-error expected (value r))
                       0.0)))
        worst (apply max errors)]
    (println (format "cagr           worst relative error %.4g over %d cases"
                     worst (count cagr-cases)))
    (is (< worst tolerance)
        (str "cagr drifted past " tolerance ": " worst))))

(deftest upper-bound-rate-agrees-within-tolerance
  (let [errors (for [n trial-counts c confidences
                     :let [expected (d/upper-bound-rate-from-zero-events n :confidence c)
                           r (port 'upper-bound-rate-from-zero-events n c)]]
                 (do (is (ok? r) (str "port refused a valid (n,confidence): " [n c]))
                     (relative-error expected (value r))))
        worst (apply max errors)]
    (println (format "upper-bound    worst relative error %.4g over %d cases"
                     worst (* (count trial-counts) (count confidences))))
    (is (< worst tolerance)
        (str "upper-bound-rate drifted past " tolerance ": " worst))))

(deftest pow-agrees-within-tolerance
  (testing "pow is compared against Math/pow directly -- it is the kernel the
  two tolerant measures above are built on, and pinning it separately says
  which of the three moved when one of them moves"
    (let [cases (for [base [0.05 0.5 0.999 1.0 1.7 3.0 97.0 1e6]
                      e    [-3.0 -0.5 0.0 0.01 0.5 1.0 2.0 7.0]]
                  [base e])
          errors (for [[base e] cases]
                   (relative-error (Math/pow base e) (port 'pow base e)))
          worst (apply max errors)]
      (println (format "pow            worst relative error %.4g over %d cases"
                       worst (count cases)))
      (is (< worst tolerance)
          (str "pow drifted past " tolerance ": " worst)))))

;; --- refusal parity ---------------------------------------------------------

(deftest both-sides-refuse-the-same-inputs
  (testing "cagr refuses a non-positive start, end or year count"
    (doseq [args [[0.0 100.0 5.0] [-3.0 100.0 5.0] [100.0 0.0 5.0]
                  [100.0 -1.0 5.0] [100.0 200.0 0.0] [100.0 200.0 -2.0]]]
      (is (apply cljc-refuses? d/cagr args) (str "cljc computed " args))
      (is (false? (ok? (apply port 'cagr args))) (str "port computed " args))))

  (testing "real-growth refuses when the denominator vanishes"
    (doseq [inflation [-1.0 -1.5 -100.0]]
      (is (cljc-refuses? d/real-growth 0.05 inflation))
      (is (false? (ok? (port 'real-growth 0.05 inflation))))))

  (testing "upper-bound-rate refuses a non-positive n or an out-of-range confidence"
    (doseq [[n c] [[0 0.95] [-5 0.95]]]
      (is (cljc-refuses? d/upper-bound-rate-from-zero-events n :confidence c))
      (is (false? (ok? (port 'upper-bound-rate-from-zero-events n c)))))
    (doseq [c [0.0 1.0 -0.5 1.5]]
      (is (cljc-refuses? d/upper-bound-rate-from-zero-events 100 :confidence c))
      (is (false? (ok? (port 'upper-bound-rate-from-zero-events 100 c))))))

  (testing "leverage-base refuses a tractability outside [0,1]"
    (doseq [t [-0.01 1.01 -5.0 42.0]]
      (is (cljc-refuses? d/leverage-score {:band :band/A :tractability t}))
      (is (false? (ok? (port 'leverage-base :band/A t)))))))

;; --- evidence floor ---------------------------------------------------------
;; Root ADR-2608136000, question 1: what does this gate return when it has no
;; input? Without this, a corpus that silently became empty -- a `for` whose
;; binding vector lost a value, a def that got shadowed -- would run zero
;; comparisons and report the same green as a run that compared everything.
;; The counts are asserted, not printed, so shrinking a corpus is a deliberate
;; edit to this test rather than an unobserved side effect of editing another.

(deftest corpus-is-not-empty
  (is (= 5 (count bands)))
  (is (= 10 (count tractabilities)))
  (is (= 1125 (count loop-shapes)))
  (is (= 64 (count growth-pairs)))
  (is (= 125 (count cagr-cases)))
  (is (= 45 (* (count trial-counts) (count confidences))))
  (is (pos? (count port-source))
      "the port source failed to load, which must not read as agreement"))
