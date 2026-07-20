(ns dynamics.core
  "Stock-flow-loop system dynamics primitives + Meadows leverage-point scoring.

   Pure data in, pure data out: zero network I/O, zero vendor SDK, models only
   (kotoba-lang layer-test admissible per ADR-2606302300 §repo-creation-check).

   No entity is categorically out of scope for this model (ADR-2607203000):
   the schema below admits ANY organization/system as a :dynamics/entity given
   the minimum stock+flow facts. What is finite today is which entities have
   been fed real data, not which entities the model can represent -- coverage
   grows as more entities are observed, it is never capped by policy.

   'Computed' always means: instantiated against real, sourced, dated facts.
   Never a fabricated global total presented as measured.")

;; ---------------------------------------------------------------------------
;; Meadows leverage points, collapsed into 5 practical bands
;; ---------------------------------------------------------------------------

(def meadows-bands
  "Donella Meadows, 'Leverage Points: Places to Intervene in a System' (1999).
   12 tiers collapsed to 5 bands for practical scoring. Weight is an ordinal
   approximation of Meadows' own claim that higher points are non-linearly
   more powerful per unit of effort -- not a measured physical constant.
   Treat every score in this namespace as an auditable heuristic, not a
   physics-grade computation: the formula is transparent, the inputs are
   dated and sourced where real, and estimates are flagged as such."
  {:band/E {:tiers [11 12] :label "constants / parameters / buffers"     :weight 1}
   :band/D {:tiers [9 10]  :label "stock-flow structure / delays"        :weight 3}
   :band/C {:tiers [7 8]   :label "feedback loop strength / gain"        :weight 5}
   :band/B {:tiers [5 6]   :label "rules / information-flow structure"   :weight 7}
   :band/A {:tiers [2 3 4] :label "goals / self-organization / paradigm" :weight 10}})

(defn band-weight [band]
  (get-in meadows-bands [band :weight]))

;; ---------------------------------------------------------------------------
;; Intervention scoring
;; ---------------------------------------------------------------------------

(defn leverage-score
  "score = band-weight * tractability (0-1), computed against structure only.

   Pool-tap interventions (those whose value depends on tapping an EXTERNAL
   population/flow) additionally carry :pool-size. Their TRUE expected yield
   is pool-size * conversion-rate, and conversion-rate is data the entity may
   not have measured yet -- in that case :expected-yield is explicitly
   :uncomputable-until-measured rather than silently treated as zero or
   omitted. This is the load-bearing distinction in the whole model: a large
   pool with an unmeasured conversion rate is not comparable to a small pool
   with a measured one, and pretending otherwise is the single most common
   way this kind of analysis goes wrong."
  [{:keys [band tractability pool-size conversion-rate] :as intervention}]
  {:pre [(contains? meadows-bands band) (<= 0 tractability 1)]}
  (let [base (* (band-weight band) tractability)]
    (cond-> (assoc intervention :base-score base)
      pool-size (assoc :addressable-pool pool-size)
      (and pool-size (number? conversion-rate))
      (assoc :expected-yield (* pool-size conversion-rate))
      (and pool-size (nil? conversion-rate))
      (assoc :expected-yield :uncomputable-until-measured))))

(defn rank-interventions
  "Sort a collection of intervention maps by :base-score descending.
   Structural interventions (no :pool-size) and pool-tap interventions
   (with :pool-size) are returned together but tagged with :kind so callers
   can choose not to compare them on the same axis -- see leverage-score."
  [interventions]
  (->> interventions
       (map leverage-score)
       (map (fn [m] (assoc m :kind (if (:addressable-pool m) :pool-tap :structural))))
       (sort-by :base-score >)))

;; ---------------------------------------------------------------------------
;; Stock / Flow / Loop -- the entity-level shape
;; ---------------------------------------------------------------------------

(defn stock
  "A single accumulation. value must be a real, dated, sourced number (or 0) --
   never a placeholder. as-of and source are required so every number in this
   model is independently checkable, the same discipline ADR-2607202700 and
   ADR-2607202800 established by hand before this library existed."
  [{:keys [id label value unit as-of source] :as m}]
  {:pre [(some? id) (number? value) (some? as-of) (some? source)]}
  (assoc m :dynamics/type :stock))

(defn flow
  [{:keys [id label from to rate-estimate unit] :as m}]
  (assoc m :dynamics/type :flow))

(defn loop*
  "kind is :reinforcing or :balancing. gain may be :unmeasured -- a loop whose
   flows include an F with no observed conversion event is legitimately
   :unmeasured, not 0. Silently defaulting an unmeasured gain to 0 would
   understate the loop; defaulting it to a guessed positive number would
   fabricate data. :unmeasured is the honest third state."
  [{:keys [id kind stocks flows gain cycle-time-days] :as m}]
  {:pre [(#{:reinforcing :balancing} kind)]}
  (assoc m :dynamics/type :loop))

;; ---------------------------------------------------------------------------
;; Loop-archetype structural strength -- WHY some loops compound faster
;; ---------------------------------------------------------------------------

(defn loop-structural-strength
  "A composite score for how fast a loop compounds, independent of its moral
   content. This is the diagnostic core of ADR-2607203000's 'why do extractive
   systems win' analysis: the same formula applied uniformly to every
   archetype in `loop-archetypes` shows that surveillance-adtech,
   speculative-derivatives and MLM-recruitment loops score high for
   STRUCTURAL reasons (short cycle time, near-total instrumentation, near-total
   self-funding, near-zero friction) that are separable from what makes them
   extractive (asymmetric information, exploitative friction design, no
   internal safety governor). The structural techniques can be adopted by a
   values-aligned org without adopting the exploitative content.

   cycle-time-days: mean time for one loop cycle to complete
   self-funding-coefficient [0,1]: does 1 unit of loop output fund >=1 unit of
     the next cycle's input? (ad revenue -> more targeting infra = high;
     tithe -> growth infra = near zero until adherents exist)
   instrumentation-completeness [0,1]: is the conversion rate that drives the
     loop actually measured? (ad click-through/LTV = near total; an org that
     has never run the experiment = 0, not 'probably fine')
   friction [0,1]: cost to the counterparty of completing one loop cycle
     (one-click social engagement = near 0; DID+WebAuthn+on-chain SBT mint = high)

   Returns nil (not a number) when cycle-time-days is not a number -- e.g. a
   loop whose gain has literally never fired has no observed cycle time, and
   a strength score computed from a guessed cycle time would be fiction."
  [{:keys [cycle-time-days self-funding-coefficient instrumentation-completeness friction]}]
  (when (number? cycle-time-days)
    (let [cycles-per-year (/ 365.0 cycle-time-days)]
      (* cycles-per-year
         (+ 0.1 self-funding-coefficient)
         (+ 0.1 instrumentation-completeness)
         (- 1.1 friction)))))

(def loop-archetypes
  "Named, sourced comparison set (ADR-2607203000). Real published figures are
   cited in :source; anything not backed by a citation carries :estimate? true
   and should be read as an order-of-magnitude placeholder, not a measurement.
   This catalog is meant to grow -- adding an archetype is adding a map, not
   changing the schema."
  {:surveillance-capitalism-adtech
   {:cycle-time-days 0.0007 ;; ~1-minute ad-auction/engagement cycle
    :self-funding-coefficient 0.95
    :instrumentation-completeness 0.98
    :friction 0.05
    :annual-flow-usd 7.8e11
    :source "eMarketer/Statista/Precedence Research 2025: global digital ad spend ~$750-800B/yr"}

   :speculative-crypto-derivatives
   {:cycle-time-days 0.0001 ;; sub-hour, frequently seconds
    :self-funding-coefficient 0.9
    :instrumentation-completeness 0.95
    :friction 0.03
    :annual-flow-usd 8.57e13
    :source "CoinGlass 2025 Crypto Derivatives Outlook: $85.7T annual volume, $264.5B/day avg"}

   :mlm-recruitment
   {:cycle-time-days 3
    :self-funding-coefficient 0.7
    :instrumentation-completeness 0.85
    :friction 0.2
    :annual-flow-usd 2.1e11
    :participants 1.8e8
    :source "WFDSA / industry reports 2025: ~$207-223B revenue, 180M+ participants, 100+ countries"}

   :jehovahs-witnesses-evangelism
   {:cycle-time-days 30 ;; monthly field-service reporting cadence, historically
    :self-funding-coefficient 0.3
    :instrumentation-completeness 0.9 ;; org has decades of measured publisher/study/baptism stats
    :friction 0.45 ;; systematic study + congregational commitment, not one-click
    :active-publishers-peak 9.2e6
    :baptisms-per-year 304500
    :home-bible-studies-per-year 7.6e6
    :memorial-attendance 2.05e7
    :source "jw.org 2025 Service Year Report; note: field-service hours no longer publicly reported as of 2023"}

   :quaker-consensus-membership
   {:cycle-time-days 365
    :self-funding-coefficient 0.05
    :instrumentation-completeness 0.2
    :friction 0.55
    :world-members 3.8e5
    :estimate? true
    :source "FWCC 2017 worldwide census: ~380,000 members; Africa 52%, N. America 20% (declining), Caribbean+LatAm 15%, Europe+ME 7%, Asia-Pacific 6%"}

   :public-goods-quadratic-funding
   {:cycle-time-days 180 ;; ~2 funding rounds/year
    :self-funding-coefficient 0.15
    :instrumentation-completeness 0.6
    :friction 0.35
    :annual-flow-usd 1.0e7
    :source "Gitcoin: $60M+ cumulative to 3,700+ projects since 2019 (~$10M/yr avg)"}

   :etzhayyim-adherent-loop
   {:cycle-time-days nil ;; F2 has never fired -- no observed cycle exists yet
    :self-funding-coefficient 0.02 ;; tithe only activates after adherents exist; currently 0
    :instrumentation-completeness 0.0 ;; conversion rate has literally never been measured
    :friction 0.8 ;; DID + WebAuthn passkey + on-chain SBT mint
    :annual-flow-usd 0
    :adherents 0
    :source "orgs/etzhayyim/root/MEMBERS.md, 2026-07-20: roster empty, 'awaiting first member'"}

   :online-gambling
   {:cycle-time-days 0.01 ;; live/in-play bets resolve in minutes to hours
    :self-funding-coefficient 0.9 ;; house-edge revenue funds acquisition/marketing directly
    :instrumentation-completeness 0.97 ;; player-tracking/personalization is industry-standard
    :friction 0.05 ;; one-tap mobile betting
    :annual-flow-usd 9.5e10
    :source "Custom Market Insights / Precedence / Statista 2025 (range $88-105B; $95B midpoint used)"}

   :wikimedia-commons
   {:cycle-time-days 180 ;; ~semiannual banner fundraising campaign cadence
    :self-funding-coefficient 0.2 ;; donations fund infra that serves more readers, not more donors directly
    :instrumentation-completeness 0.75 ;; famous for rigorous banner A/B-testing of conversion rate
    :friction 0.3 ;; donation form + payment, but no account/identity required
    :annual-flow-usd 2.086e8
    :donors 7.0e6
    :source "Wikimedia Foundation FY2024-2025 audit report: $208.6M revenue, 7M+ donors, 66M+ articles"}

   :linux-foundation-membership
   {:cycle-time-days 365 ;; annual membership dues cycle
    :self-funding-coefficient 0.4 ;; recurring institutional dues fund services that retain/attract members
    :instrumentation-completeness 0.5 ;; precise financial reporting, but not a conversion-funnel practice
    :friction 0.5 ;; paid membership tier negotiation, not a one-click join
    :annual-flow-usd 3.113e8
    :member-orgs 3000
    :source "Linux Foundation Annual Report 2025: $311.3M gross revenue, 3,000+ member organizations"}

   :givewell-effective-altruism
   {:cycle-time-days 90 ;; quarterly-ish major grant rounds; grants approved somewhat continuously
    :self-funding-coefficient 0.25 ;; donations fund research capacity that improves recommendations, attracting more donors
    :instrumentation-completeness 0.65 ;; publishes precise money-moved/donor-count metrics annually (distinct from its separately-famous cost-per-outcome impact rigor, not modeled here)
    :friction 0.3 ;; a donation decision among evaluated top charities, not one-click
    :annual-flow-usd 4.18e8
    :donors 3.0e4
    :source "GiveWell 2025 grantmaking year (Feb 2025-Jan 2026): $418M approved, 131 grants to 69 orgs; 2024 metrics year: 30,000+ donors"}

   :global-fossil-fuel-industry
   {:cycle-time-days 90 ;; quarterly capital-allocation/reinvestment cycle (earnings-driven capex decisions), not the daily commodity-trading cycle -- the loop modeled here is reinvestment into more extraction capacity, not spot-market turnover
    :self-funding-coefficient 0.6 ;; revenue directly funds further exploration/extraction capex, a well-documented reinvestment flywheel, tempered by long physical lead times vs digital reinvestment
    :instrumentation-completeness 0.9 ;; production/reserves/output are measured with extreme precision industry-wide (barrels, cubic meters)
    :friction 0.1 ;; end-consumer purchase (fuel, electricity) is near-frictionless
    :annual-flow-usd 8.32e12
    :source "Precedence Research 2025: global fossil fuels market ~$8.32T"}})

(defn compare-archetypes
  "Structural-strength ranking over every archetype with a numeric cycle time.
   Archetypes with cycle-time-days nil (never-fired loops) are returned
   separately under :unmeasured rather than silently dropped or scored as 0 --
   that gap IS the finding, not noise to filter out."
  ([] (compare-archetypes loop-archetypes))
  ([archetypes]
   (let [scored (for [[k v] archetypes
                       :let [s (loop-structural-strength v)]]
                  [k s])]
     {:ranked (->> scored (remove (comp nil? second)) (sort-by second >))
      :unmeasured (->> scored (filter (comp nil? second)) (map first))})))
