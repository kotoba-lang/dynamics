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
      ;; Previously this asserted a specific archetype was rank 1
      ;; (:speculative-crypto-derivatives). That is a fact about which entries
      ;; happen to be IN the catalog, not about the partitioning behaviour this
      ;; test is named for, so it broke the moment a faster loop was added
      ;; (:solana-fee-loop, 400ms slot vs the derivative desk's ~1 day). The
      ;; catalog's own docstring promises "adding an archetype is adding a map",
      ;; so a test that fails on a legitimate addition is testing the wrong
      ;; thing. Assert the ordering invariant instead, which is what :ranked
      ;; actually guarantees.
      (is (seq ranked))
      (let [scores (map second ranked)]
        (is (every? number? scores))
        (is (= scores (reverse (sort scores))))))))

(deftest value-accrual-cohort-triple-is-internally-consistent-test
  (testing "every value-accrual entry carries fees, holders revenue, a share that
            matches them, and a dated source -- so 'who captures the fee' is a
            measurement rather than a mechanism story"
    (let [cohort (into {} (filter (fn [[_ v]] (contains? v :holders-share-of-fees))
                                  d/loop-archetypes))]
      ;; 9 added 2026-07-31 for the owner's crypto comparison, plus the triple
      ;; retrofitted onto the pre-existing :ethereum-network-fee-loop
      (is (= 10 (count cohort)))
      (doseq [[id v] cohort]
        (testing (str id)
          (is (number? (:fees-1y-usd v)))
          (is (number? (:holders-revenue-1y-usd v)))
          (is (string? (:source v)))
          ;; every figure in this cohort comes from one source read on one date;
          ;; mixing dates would make the ratios incomparable
          (is (re-find #"2026-07-31" (:source v)))
          ;; the share must be recomputable from the two measurements it summarises
          (let [{:keys [fees-1y-usd holders-revenue-1y-usd holders-share-of-fees]} v]
            (when (pos? fees-1y-usd)
              (is (< (Math/abs (- holders-share-of-fees
                                  (/ holders-revenue-1y-usd fees-1y-usd)))
                     0.01)
                  "stated share must match measured fees/holders-revenue")))))
      ;; ICP is the one entry where holders revenue EXCEEDS fees; it must say so
      ;; in-band rather than be silently clamped to <=1 (methodology rule from
      ;; ADR-2607259800: report the anomaly, do not normalise it away)
      (let [icp (:internet-computer-cycles-burn d/loop-archetypes)]
        (is (string? (:caveat icp)))
        (is (> (:holders-revenue-1y-usd icp) (:fees-1y-usd icp))))
      ;; the control cases: zero accrual is 0, never nil, never absent
      (doseq [id [:monero-no-fee-capture :gnosis-chain-fee-loop]]
        (is (zero? (:holders-revenue-1y-usd (id d/loop-archetypes))))
        (is (zero? (:holders-share-of-fees (id d/loop-archetypes))))))))

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
      (testing ":dates are CHRONOLOGICAL, which since the catalog reached BCE is
                no longer the same thing as string-sorted -- 'c. 2400 BCE' must
                precede 'c. 1754 BCE'"
        (doseq [[param v] tl]
          (is (= (:dates v) (mapv :date (d/regime-changes {:changed param})))
              (str param " dates must match regime-changes' own ordering")))))))

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

;; ── measured loop gain from adjacent indicators (2026-07-25) ─────────────

(deftest cagr-is-nil-not-zero-when-undefined-test
  (is (nil? (d/cagr 0 100 10)) "a non-positive start has no meaningful ratio")
  (is (nil? (d/cagr 100 0 10)) "nor does a non-positive end")
  (is (nil? (d/cagr 100 200 0)) "nor a zero-year window")
  (is (nil? (d/cagr nil 200 10)))
  (testing "and a real doubling over 10 years is ~7.18%/yr"
    (is (< (Math/abs (- (d/cagr 100 200 10) 0.0717734625)) 1e-9))))

(deftest real-growth-uses-the-exact-formula-not-the-subtraction-test
  (testing "at low inflation the approximation is close, so the test that
            matters is the high-inflation one"
    (is (< (Math/abs (- (d/real-growth 0.07 0.02) 0.049019607843)) 1e-9)))
  (testing "at 200% inflation, nominal-minus-inflation would say -100% real
            growth for a loop that nominally tripled; the exact formula says
            -66.7%. This catalog contains economies at both ends of that range,
            so the approximation would systematically distort exactly where the
            distinction matters most"
    (let [exact (d/real-growth 2.0 2.0)
          approx (- 2.0 2.0)]
      (is (< (Math/abs (- exact 0.0)) 1e-12) "tripling under tripling prices is 0% real")
      (is (= 0.0 approx) "here they agree...")
      (let [exact2 (d/real-growth 1.0 2.0)
            approx2 (- 1.0 2.0)]
        (is (< (Math/abs (- exact2 (- (/ 2.0 3.0) 1.0))) 1e-12) "...and here they do not")
        (is (= -1.0 approx2))
        (is (> exact2 approx2) "the approximation understates real growth under high inflation"))))
  (testing "nil rather than a wrong number when inflation wipes out the denominator"
    (is (nil? (d/real-growth 0.05 -1.0)))
    (is (nil? (d/real-growth 0.05 nil)))))

(deftest money-loop-measures-never-fabricates-a-missing-quantity-test
  (testing "every derived quantity is nil when its own inputs are missing --
            partial data yields partial answers, not zeros"
    (let [m (d/money-loop-measures {:broad-money-start 100 :broad-money-end 200 :years 10})]
      (is (some? (:nominal-growth m)))
      (is (nil? (:real-growth m)) "no inflation supplied")
      (is (nil? (:monetization m)))
      (is (nil? (:credit-intensity m)))
      (is (nil? (:price m)))))
  (testing "a fully-supplied economy yields every quantity"
    (let [m (d/money-loop-measures {:broad-money-start 100 :broad-money-end 200 :years 10
                                    :inflation-annual-pct 3.0
                                    :broad-money-pct-gdp 120.0
                                    :private-credit-pct-gdp 90.0
                                    :lending-rate-pct 5.5})]
      (is (< (Math/abs (- (:monetization m) 1.2)) 1e-12))
      (is (< (Math/abs (- (:private-credit-share m) 0.9)) 1e-12))
      (is (< (Math/abs (- (:credit-intensity m) 0.75)) 1e-12))
      (is (< (Math/abs (- (:price m) 0.055)) 1e-12))
      (is (< (:real-growth m) (:nominal-growth m)) "deflating a positive inflation lowers it")))
  (testing "the nominal figure is always accompanied by its deflated twin when
            inflation is known -- ranking on nominal LCU growth across
            currencies is the way this analysis goes wrong"
    (let [m (d/money-loop-measures {:broad-money-start 100 :broad-money-end 400 :years 5
                                    :inflation-annual-pct 40.0})]
      (is (some? (:nominal-growth m)))
      (is (some? (:real-growth m)))
      (is (< (:real-growth m) 0) "a nominal quadrupling under 40%/yr inflation is real SHRINKAGE"))))


;; ── historical depth, and this catalog's own two corrections ─────────────

(deftest the-catalog-does-not-start-at-2008-or-1694-test
  (testing "it started at QE1, was extended to 1694, and both were wrong in the
            same way -- taking the most familiar instance for the first one"
    (let [all (d/regime-changes)
          earliest (first all)]
      (is (= :lagash-temple-palace (:institution earliest)))
      (is (= -2400 (:year earliest)))
      (is (<= 28 (count all)))))
  (testing "chronological order survives BCE, bare years and full dates together"
    (let [ks (map (fn [e] (or (:year e)
                              #?(:clj (Long/parseLong (subs (:date e) 0 4))
                                 :cljs (js/parseInt (subs (:date e) 0 4) 10))))
                  (d/regime-changes))]
      (is (= (seq ks) (sort ks))))))

(deftest bank-of-england-was-not-first-and-the-entry-says-so-test
  (testing "Sveriges Riksbank (1668) predates the BoE (1694) by 26 years and is
            the oldest surviving central bank"
    (let [riks (first (filter #(= :sveriges-riksbank (:institution %)) (d/regime-changes)))
          boe (first (filter #(= :bank-of-england (:institution %)) (d/regime-changes)))]
      (is (= 1668 (:year riks)))
      (is (= 1694 (:year boe)))
      (is (< (:year riks) (:year boe)))))
  (testing "and Europe's first banknotes were Stockholms Banco's in 1661, by an
            institution that then failed"
    (let [sb (first (filter #(= :stockholms-banco (:institution %)) (d/regime-changes)))]
      (is (= 1661 (:year sb)))))
  (testing "the BoE entry records the correction rather than quietly dropping it"
    (let [boe (first (filter #(= :bank-of-england (:institution %)) (d/regime-changes)))]
      (is (re-find #"CORRECTED" (:detail boe)))
      (is (re-find #"wrong twice over" (:detail boe))))))

(deftest paper-money-and-fiat-both-predate-europe-test
  (testing "government paper money is Song Chinese, 1024 -- 670 years before the BoE"
    (let [jiaozi (first (filter #(= :song-government (:institution %)) (d/regime-changes)))]
      (is (= 1024 (:year jiaozi)))
      (is (= :china (:jurisdiction jiaozi)))))
  (testing "and a pure fiat standard was run empire-wide in the 13th century, which
            is what qualifies 1971 from 'unprecedented' to 'first GLOBAL instance'"
    (let [yuan (first (filter #(= :yuan-government (:institution %)) (d/regime-changes)))
          nixon (first (filter #(= "1971-08-15" (:date %)) (d/regime-changes)))]
      (is (= 1260 (:year yuan)))
      (is (= :self-funding-coefficient (:changed yuan)))
      (is (= :self-funding-coefficient (:changed nixon)))
      (is (re-find #"QUALIFIED" (:detail nixon)))
      (is (re-find #"first GLOBAL removal" (:detail nixon)))
      (is (< (:year yuan) 1971)))))

(deftest japan-invented-qe-seven-years-before-the-fed-test
  (testing "the 2008 entries treat QE as novel; it was run in Tokyo from 2001 and
            lifted in 2006, before the Fed started"
    (let [boj-qe (first (d/regime-changes {:since "2001-01-01" :until "2001-12-31"}))
          fed-qe1 (first (d/regime-changes {:since "2008-01-01" :until "2008-12-31"}))]
      (is (= "2001-03-19" (:date boj-qe)))
      (is (= :bank-of-japan (:institution boj-qe)))
      (is (= "2008-11-25" (:date fed-qe1)))
      (is (not (:estimate? boj-qe)) "primary-sourced (SF Fed / IMF / BOJ)"))))

(deftest the-only-debt-reversing-mechanism-is-the-oldest-one-test
  (testing "clean slates are the sole entries whose :changed is :stock-reset, and
            nothing after 1400 BCE has one -- the modern half of this catalog has
            no mechanism that systematically reverses a credit stock"
    (let [resets (filter #(= :stock-reset (:changed %)) (d/regime-changes))]
      (is (= 2 (count resets)))
      (is (every? #(neg? (:year %)) resets))
      (is (every? #(= :mesopotamia (:jurisdiction %)) resets))))
  (testing "the nearest modern analogue is recorded as explicitly partial"
    (let [qt (first (filter #(= "2025-12-01" (:date %)) (d/regime-changes)))]
      (is (re-find #"HALF" (:detail qt))))))

(deftest history-widens-who-and-what-moved-test
  (testing "institutions and jurisdictions both grow once real history is included"
    (let [insts (into #{} (map :institution) (d/regime-changes))
          juris (into #{} (map :jurisdiction) (d/regime-changes))]
      (is (contains? juris :china))
      (is (contains? juris :mesopotamia))
      (is (contains? juris :sweden))
      (is (contains? juris :global))
      (is (<= 12 (count insts)))))
  (testing "self-funding-coefficient now spans four millennia and starts in China,
            not Europe"
    (let [sf (:self-funding-coefficient (d/parameter-timeline))]
      (is (<= 10 (:count sf)))
      (is (= "1260s" (first (:dates sf))))
      (is (contains? (:institutions sf) :yuan-government)))))
