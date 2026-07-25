(ns dynamics.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [dynamics.core :as d]))

(deftest meadows-bands-ordering-test
  (testing "higher bands carry strictly higher weight (Meadows' claim: upper points dominate)"
    (is (< (d/band-weight :band/E) (d/band-weight :band/D)
           (d/band-weight :band/C) (d/band-weight :band/B) (d/band-weight :band/A)))))

(deftest leverage-score-structural-test
  (testing "a structural intervention (no pool) scores band-weight * tractability"
    (let [scored (d/leverage-score {:id :reframe-goal :band :band/A :tractability 0.8})]
      (is (= 8.0 (:base-score scored)))
      (is (not (contains? scored :addressable-pool))))))

(deftest leverage-score-pool-tap-unmeasured-test
  (testing "a pool-tap intervention with NO conversion-rate reports :uncomputable-until-measured, not a number"
    (let [scored (d/leverage-score {:id :apply-gitcoin :band :band/D :tractability 0.7
                                     :pool-size 1028})]
      (is (= :uncomputable-until-measured (:expected-yield scored)))
      (is (= 1028 (:addressable-pool scored))))))

(deftest leverage-score-pool-tap-measured-test
  (testing "once conversion-rate is known, expected-yield is a real number"
    (let [scored (d/leverage-score {:id :apply-gitcoin :band :band/D :tractability 0.7
                                     :pool-size 1000 :conversion-rate 0.01})]
      (is (= 10.0 (:expected-yield scored))))))

(deftest rank-interventions-test
  (testing "ranking sorts by base-score descending and tags kind"
    (let [ranked (d/rank-interventions
                  [{:id :a :band :band/E :tractability 1.0}
                   {:id :b :band :band/A :tractability 1.0}
                   {:id :c :band :band/D :tractability 1.0 :pool-size 100}])]
      (is (= [:b :c :a] (map :id ranked)))
      (is (= :structural (:kind (first (filter #(= :a (:id %)) ranked)))))
      (is (= :pool-tap (:kind (first (filter #(= :c (:id %)) ranked))))))))

(deftest loop-structural-strength-extractive-beats-etzhayyim-test
  (testing "surveillance-adtech scores far higher than etzhayyim's never-fired loop -- but etzhayyim's
            loop returns nil (unmeasured), never a fabricated low number"
    (let [adtech (d/loop-structural-strength (:surveillance-capitalism-adtech d/loop-archetypes))
          etz (d/loop-structural-strength (:etzhayyim-adherent-loop d/loop-archetypes))]
      (is (number? adtech))
      (is (pos? adtech))
      (is (nil? etz)))))

(deftest compare-archetypes-partitions-measured-vs-unmeasured-test
  (testing "etzhayyim's loop lands in :unmeasured, not silently scored 0 and dropped"
    (let [{:keys [ranked unmeasured]} (d/compare-archetypes)]
      (is (some #{:etzhayyim-adherent-loop} unmeasured))
      (is (not (some #(= :etzhayyim-adherent-loop (first %)) ranked)))
      (is (= :speculative-crypto-derivatives (ffirst ranked))))))

(deftest jehovahs-witnesses-archetype-has-real-sourced-figures-test
  (testing "the JW archetype carries dated, sourced, real published figures -- not estimates"
    (let [jw (:jehovahs-witnesses-evangelism d/loop-archetypes)]
      (is (not (:estimate? jw)))
      (is (= 304500 (:baptisms-per-year jw)))
      (is (string? (:source jw))))))

(deftest upper-bound-rate-from-zero-events-rule-of-three-test
  (testing "for large n, the exact bound is well-approximated by the rule-of-three (~3/n at 95%)"
    (let [n 16497
          exact (d/upper-bound-rate-from-zero-events n)
          rule-of-three (/ 3.0 n)]
      (is (< (Math/abs (- exact rule-of-three)) 1e-6))))
  (testing "a larger trial count tightens (lowers) the upper bound"
    (is (> (d/upper-bound-rate-from-zero-events 100)
           (d/upper-bound-rate-from-zero-events 100000))))
  (testing "a higher confidence level raises the upper bound"
    (is (< (d/upper-bound-rate-from-zero-events 1000 :confidence 0.90)
           (d/upper-bound-rate-from-zero-events 1000 :confidence 0.99)))))

;; ---------------------------------------------------------------------------
;; Incumbent money-system comparators + the second (scale) axis, added
;; 2026-07-25 so "how does this token economy compare to central banking and
;; card networks?" has an admissible computation instead of an assertion.
;; ---------------------------------------------------------------------------

(deftest incumbent-money-comparators-present-and-sourced-test
  (testing "every incumbent money-system comparator carries a real citation"
    (doseq [k [:visa-card-network-interchange :commercial-bank-credit-creation
               :central-bank-balance-sheet-expansion :ethereum-network-fee-loop
               :stablecoin-reserve-yield :wir-bank-mutual-credit
               :holochain-holofuel-mutual-credit :engi-en-mutual-credit-current]]
      (is (contains? d/loop-archetypes k) (str k " missing from the catalog"))
      (is (string? (:source (k d/loop-archetypes))) (str k " has no :source"))))
  (testing "a monetary authority -- and ONLY a monetary authority -- funds its next
            cycle without earning anything AND has a counterparty who cannot decline.
            Resolving the aggregate into named institutions (2026-07-25) turned this
            from a claim about one entry into a claim about a class, which is the
            stronger statement: the property tracks the institution type, not the
            modelling choice."
    (let [unbounded (into #{} (comp (filter (fn [[_ v]] (and (= 1.0 (:self-funding-coefficient v))
                                                             (= 0.0 (:friction v)))))
                                    (map key))
                          d/loop-archetypes)]
      (is (= #{:central-bank-balance-sheet-expansion
               :fed-balance-sheet
               :bank-of-japan-balance-sheet
               :ecb-balance-sheet}
             unbounded)
          "exactly the monetary authorities, and nothing else in the catalog")
      (testing "the PBoC is deliberately NOT in that set: a directed loan still has a
                borrower, so its friction is 0.05 rather than 0. Legal tender has no
                counterparty who can decline; a loan quota still needs someone to lend to."
        (is (= 0.05 (:friction (:pboc-directed-credit-creation d/loop-archetypes))))
        (is (not (contains? unbounded :pboc-directed-credit-creation))))
      (testing "and commercial banks are not in it either -- they must earn the capital
                that permits the next cycle"
        (is (not (contains? unbounded :commercial-bank-credit-creation)))
        (is (not (contains? unbounded :us-commercial-bank-credit-creation)))))))

(deftest china-is-the-largest-money-creation-loop-test
  (testing "the catalog did not contain the largest actor in the system it models
            until 2026-07-25. China's M2 is ~2.3x US M2 at the dated FX rate used,
            and the conversion is stated so it can be re-derived rather than trusted"
    (let [cn (:pboc-directed-credit-creation d/loop-archetypes)
          us (:us-commercial-bank-credit-creation d/loop-archetypes)]
      (is (= 6.774 (:cny-per-usd cn)) "a dated rate, not an assumed one")
      (is (< 1e-9 (Math/abs (- (:m2-usd-equivalent cn)
                               (/ (:m2-cny cn) (:cny-per-usd cn)))))
          "sanity: the stated USD equivalent is the stated CNY over the stated rate")
      (is (> (:m2-usd-equivalent cn) (* 2 (:m2-usd us))))
      (testing "and its cycle is faster than a policy-meeting calendar, because its
                binding constraint is administrative rather than the price of money"
        (is (< (:cycle-time-days cn) (:cycle-time-days (:fed-balance-sheet d/loop-archetypes))))))))

(deftest regime-changes-are-dated-attributed-and-queryable-test
  (testing "every regime change names who, where, when, and which loop parameter moved"
    (doseq [c (d/regime-changes)]
      (is (string? (:date c)))
      (is (keyword? (:institution c)))
      (is (keyword? (:jurisdiction c)))
      (is (keyword? (:changed c)))
      (is (string? (:source c)))))
  (testing "filters compose and return chronological order"
    (let [boj (d/regime-changes {:institution :bank-of-japan})]
      (is (seq boj))
      (is (= (mapv :date boj) (sort (mapv :date boj))))
      (is (every? #(= :japan (:jurisdiction %)) boj)))
    (is (= 1 (count (d/regime-changes {:institution :federal-reserve :since "2025-01-01"})))
        "only QT ending falls in that window"))
  (testing "a SCHEDULED future event is returned like any other but flagged, so a
            caller can exclude it -- never silently, because 'what is committed' and
            'what has occurred' are both real questions"
    (let [future-events (filter :scheduled? (d/regime-changes))]
      (is (= 1 (count future-events)))
      (is (= "2027-04" (:date (first future-events))))
      (is (= :bank-of-japan (:institution (first future-events))))))
  (testing "parameter-timeline shows which parameters are policy rather than constants"
    (let [tl (d/parameter-timeline)]
      (is (contains? tl :self-funding-coefficient))
      (is (<= 2 (count (:institutions (:self-funding-coefficient tl))))
          "moved by more than one institution -- so it is policy, not a constant to
           estimate once")
      (is (every? (fn [[_ v]] (= (:dates v) (sort (:dates v)))) tl)))))

(deftest mutual-credit-bracket-test
  (testing "EN sits between a 92-year-old working precedent and an 8-year-old stalled one --
            WIR has fired (numeric strength), Holochain and EN have not (nil, never 0)"
    (is (number? (d/loop-structural-strength (:wir-bank-mutual-credit d/loop-archetypes))))
    (is (nil? (d/loop-structural-strength (:holochain-holofuel-mutual-credit d/loop-archetypes))))
    (is (nil? (d/loop-structural-strength (:engi-en-mutual-credit-current d/loop-archetypes))))
    (let [{:keys [unmeasured]} (d/compare-archetypes)]
      (is (some #{:holochain-holofuel-mutual-credit} unmeasured))
      (is (some #{:engi-en-mutual-credit-current} unmeasured)))))

(deftest compare-archetypes-2d-refuses-to-rank-across-flow-kinds-test
  (testing "flows are grouped by kind, never pooled into one ranking"
    (let [{:keys [by-flow-kind unclassified-flow-kind n-both-known]} (d/compare-archetypes-2d)]
      (is (< 1 (count by-flow-kind)) "more than one flow kind must be represented")
      (is (empty? unclassified-flow-kind)
          "every archetype carrying :annual-flow-usd must declare what kind of flow it is")
      (is (pos? n-both-known))
      (testing "gross settled volume and fees collected are not mixed"
        (is (some #{:visa-card-network-interchange}
                  (map :id (:gross-volume-settled by-flow-kind))))
        (is (some #{:ethereum-network-fee-loop}
                  (map :id (:fees-collected by-flow-kind))))))))

(deftest speed-does-not-determine-scale-test
  (testing "within a single flow kind, the fastest-compounding loop is not the largest --
            the concrete counterexample the second axis exists to surface"
    (let [{:keys [by-flow-kind speed-vs-scale-correlation]} (d/compare-archetypes-2d)
          rev (:operator-revenue by-flow-kind)
          biggest-flow (first rev)                      ;; sorted by flow desc
          fastest (first (sort-by :strength > rev))]
      (is (= :surveillance-capitalism-adtech (:id biggest-flow)))
      (is (= :surveillance-capitalism-adtech (:id fastest)))
      ;; ...but the SECOND largest by revenue is near the bottom by speed
      (is (= :mlm-recruitment (:id (second rev))))
      (is (< (:strength (second rev)) (:strength (nth rev 2))))
      (testing "per-kind correlations are reported with their n, never pooled across kinds"
        (doseq [[_ {:keys [spearman n]}] speed-vs-scale-correlation]
          (is (<= 3 n))
          (is (<= -1.0 spearman 1.0)))))))
