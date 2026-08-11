(ns dynamics.xmile
  "Real stock-and-flow simulation via kotoba-lang/org-oasis-open-xmile (OASIS
   XMILE 1.0), for the loops this namespace's own `loop-structural-strength`
   can only score as a single point-in-time number.

   This namespace exists to correct a real gap: `org-oasis-open-xmile` was
   already ADR-authoritative for system-dynamics computation in kotoba-lang
   (ADR-2607072350, 2026-07-07) when `dynamics.core` was built (ADR-2607203000,
   2026-07-20) -- `dynamics.core`'s own hand-rolled `stock`/`flow`/`loop*`
   constructors and `loop-structural-strength` formula duplicate ground a
   real, standards-compliant, already-simulating engine already covered, more
   rigorously. `loop-structural-strength` stays (it answers a different
   question cheaply -- 'which of N archetypes compounds fastest, given only
   4 coarse parameters' -- without needing a full equation model for each
   one), but any caller who wants an actual projected TRAJECTORY, not a
   single comparative score, should reach for this namespace instead of
   inventing another one-off simulator.

   Callers MUST require `org-oasis-open-xmile`'s own `xmile.model`/
   `xmile.validate`/`xmile.execute` on their classpath (this namespace is a
   thin, honest convenience layer over them, not a reimplementation -- it
   takes an already-built `xmile-model-ns` map of those 3 namespaces' public
   fns, the same 'host injects its dependencies' pattern org-oasis-open-xmile
   itself uses for XML parsing, so this namespace has zero hard dependency
   edge of its own).")

(defn- round [x]
  #?(:cljs (js/Math.round x)
     :clj (Math/round (double x))))

(defn acquisition-model
  "Build a real XMILE model (via the `xmile-model-ns` map of {:model
   :validate :execute}, whose namespaces are xmile.model/xmile.validate/
   xmile.execute from org-oasis-open-xmile) of the simplest honest
   acquisition-funnel shape this catalog keeps re-deriving by hand: a
   constant real inflow rate (e.g. daily visitors), a constant conversion
   rate (e.g. a measured or upper-bound-estimated F2), feeding a single
   accumulating stock from a real starting value.

   This is deliberately NOT a network-effect/feedback model (the stock does
   not feed back into the inflow rate) -- adding a self-reinforcing term
   would require a real measured coefficient this catalog has never
   observed (the whole point of the F2-upper-bound finding is that F2 has
   NEVER fired), and fabricating one would violate this whole workspace's
   'never fabricate' discipline. Callers who want to explore a
   feedback/network-effect SCENARIO should build one explicitly and label
   it as a scenario, not ship it as this fn's default.

   :name              string, the XMILE model name
   :inflow-rate       real number, e.g. daily visitors (NOT weekly -- pick a
                       time unit and keep :sim-days in the same unit)
   :conversion-rate    real number in [0,1], e.g. an F2 upper bound
   :initial-stock      real number, the stock's real starting value
   :sim-days           simulation horizon, in the same time unit as inflow-rate
   opts (optional)     :dt (default 1.0), :method (default :rk4)"
  [xmile-model-ns {:keys [name inflow-rate conversion-rate initial-stock sim-days]
                    :or {}}
   & [{:keys [dt method] :or {dt 1.0 method :rk4}}]]
  (let [{:keys [model sim-specs aux flow stock add-variable]} xmile-model-ns]
    (-> (model name {:xmile/sim-specs (sim-specs 0.0 (double sim-days) {:xmile/dt dt :xmile/method method})})
        (add-variable (aux "Inflow_Rate" (str (double inflow-rate))))
        (add-variable (aux "Conversion_Rate" (str (double conversion-rate))))
        (add-variable (flow "Conversions" "Inflow_Rate * Conversion_Rate"))
        (add-variable (stock "Stock" (str (double initial-stock)) {:xmile/inflows #{"Conversions"}})))))

(defn percentage-rate-model
  "Build a real XMILE model of the complementary shape to `acquisition-model`
   above: not a constant ADDITIVE inflow, but a constant PROPORTIONAL
   (exponential) rate applied to the stock itself -- `Stock' = Stock *
   Annual_Rate`. This is the right shape for a real, already-measured
   year-over-year percentage change on a STOCK (a level, e.g. current total
   enrollment), as opposed to `acquisition-model`'s shape (a flow feeding an
   accumulator) -- using the wrong one for a given real fact is a modeling
   error even when both would 'run'. `annual-rate` may be negative (decline)
   and time is in YEARS (not days) to keep the equation legible for a rate
   that is itself already annual -- pass `:sim-days` in the caller's usual
   sense for `project`'s API but treat both as 'years' here.

   :name          string, the XMILE model name
   :initial-stock  real number, the stock's real starting value
   :annual-rate    real number (can be negative), e.g. a real YoY fractional
                    change already computed from 2 real observations
   :sim-years      simulation horizon in years
   opts (optional)  :dt (default 0.1) :method (default :rk4)"
  [xmile-model-ns {:keys [name initial-stock annual-rate sim-years]}
   & [{:keys [dt method] :or {dt 0.1 method :rk4}}]]
  (let [{:keys [model sim-specs aux flow stock add-variable]} xmile-model-ns]
    (-> (model name {:xmile/sim-specs (sim-specs 0.0 (double sim-years) {:xmile/dt dt :xmile/method method})})
        (add-variable (aux "Annual_Rate" (str (double annual-rate))))
        (add-variable (flow "Change" "Stock * Annual_Rate"))
        (add-variable (stock "Stock" (str (double initial-stock)) {:xmile/inflows #{"Change"}})))))

(defn bass-diffusion-model
  "Build a real XMILE model of Frank Bass's 1969 new-product-growth /
   innovation-diffusion model (Bass, F.M., 'A New Product Growth for Model
   Consumer Durables', Management Science 15(5), 1969) -- the standard,
   textbook system-dynamics model for adoption spreading through a bounded
   population via TWO distinct channels at once:

     Adoptions' = (p + q * Adopters/Market_Size) * (Market_Size - Adopters)

   `p` (coefficient of INNOVATION / external influence) is adoption driven
   by a source outside the adopter population itself (e.g. broadcast/mass-
   media reach, or -- for an autonomous-publication actor fleet -- content
   an evangelist AGENT posts that reaches non-adopters directly). `q`
   (coefficient of IMITATION / internal influence) is adoption driven by
   contact with EXISTING adopters -- word-of-mouth among humans, or,
   structurally analogous but a real different regime, one evangelized
   node (human OR agent) reaching another. `p` alone gives a decelerating
   curve (front-loaded, like `acquisition-model`'s constant inflow bounded
   by a shrinking pool of remaining non-adopters); a nonzero `q` gives the
   classic S-curve -- slow start, an accelerating middle phase once enough
   adopters exist to drive real word-of-mouth mass, then saturation as
   `Market_Size` is approached. This is the right model SHAPE for any real
   system where already-converted members can themselves become a
   propagation channel -- neither `acquisition-model` (no feedback at all)
   nor `percentage-rate-model` (proportional to the CURRENT stock alone,
   with no bound) captures that S-curve dynamic.

   THIS FUNCTION DOES NOT SUPPLY p/q ITSELF -- unlike `percentage-rate-
   model`'s real YoY-observed rate, a `p`/`q` pair for a loop that has
   never fired (the exact situation `dynamics.core/loop-structural-
   strength`'s docstring already flags for `etzhayyim-adherent-loop`) has
   NOTHING to be measured from. Callers exploring such a loop MUST label
   `p`/`q` as an explicit, named SCENARIO (not a claimed real rate) --
   this is the same discipline `acquisition-model`'s docstring already
   states for a feedback/network-effect extension, now given a real,
   citable, standard model to run those scenarios through instead of an
   ad-hoc one.

   :name          string, the XMILE model name
   :market-size    real number, the addressable population ceiling (M)
   :p-coefficient  external/innovation coefficient (label as measured or scenario)
   :q-coefficient  internal/imitation coefficient (label as measured or scenario)
   :initial-adopters  real number, the stock's real starting value
   :sim-time       simulation horizon, in the caller's chosen time unit
   opts (optional)  :dt (default 1.0) :method (default :rk4)"
  [xmile-model-ns {:keys [name market-size p-coefficient q-coefficient initial-adopters sim-time]}
   & [{:keys [dt method] :or {dt 1.0 method :rk4}}]]
  (let [{:keys [model sim-specs aux flow stock add-variable]} xmile-model-ns]
    (-> (model name {:xmile/sim-specs (sim-specs 0.0 (double sim-time) {:xmile/dt dt :xmile/method method})})
        (add-variable (aux "Market_Size" (str (double market-size))))
        (add-variable (aux "P_Coefficient" (str (double p-coefficient))))
        (add-variable (aux "Q_Coefficient" (str (double q-coefficient))))
        (add-variable (flow "Adoptions"
                             "(P_Coefficient + Q_Coefficient * Stock / Market_Size) * (Market_Size - Stock)"))
        (add-variable (stock "Stock" (str (double initial-adopters)) {:xmile/inflows #{"Adoptions"}})))))

(def ^:private network-effect-required-keys
  [:name :initial-independent-developers :owned-capability-providers
   :initial-independent-providers :owned-reusable-components
   :initial-independent-components :initial-active-organizations
   :initial-independent-nodes :operator-nodes :initial-verified-receipts
   :developer-market :organization-market :node-market
   :external-developer-rate :developer-network-coefficient
   :provider-productivity :component-productivity
   :external-organization-rate :stack-network-coefficient
   :external-node-rate :node-demand-coefficient
   :receipts-per-organization :receipts-per-node
   :provider-scale :component-scale :receipt-scale :node-scale
   :complement-value-weight :technical-base-units
   :provider-barrier-weight :component-barrier-weight
   :receipt-barrier-weight :organization-barrier-weight :node-barrier-weight
   :entrant-replication-throughput :entrant-cost-per-unit
   :fixed-technical-cost :sim-years])

(defn- require-network-effect-params! [params]
  (let [missing (remove #(contains? params %) network-effect-required-keys)]
    (when (seq missing)
      (throw (ex-info "network-effect-barrier-model: missing required parameters"
                      {:missing (vec missing)})))
    (doseq [k (remove #{:name} network-effect-required-keys)]
      (let [v (get params k)]
        (when-not (and (number? v) (<= 0 v))
          (throw (ex-info "network-effect-barrier-model: parameter must be a non-negative number"
                          {:parameter k :value v})))))
    (doseq [k [:developer-market :organization-market :node-market
               :provider-scale :component-scale :receipt-scale :node-scale
               :entrant-replication-throughput :sim-years]]
      (when-not (pos? (get params k))
        (throw (ex-info "network-effect-barrier-model: scale, market, throughput and horizon must be positive"
                        {:parameter k :value (get params k)}))))
    params))

(defn- num-eqn [x] (str (double x)))

(defn network-effect-barrier-model
  "Build an executable multi-stock XMILE scenario for the Kotoba stack's
   network effects AND a competitor's catch-up barrier.

   It deliberately separates owned complements (existing Kotoba capability
   providers, reusable components and operator nodes) from INDEPENDENT network
   participants. Owned repos raise present utility and replication work, but
   do not masquerade as an external network. The independent stocks are:

     Independent_Developers, Independent_Providers, Independent_Components,
     Active_Organizations, Independent_Compute_Nodes, Verified_Receipts.

   Developer adoption is reinforced by existing developers times a bounded
   complement index. Organization adoption is reinforced by existing active
   organizations times the full-stack complement index. Providers/components,
   compute nodes and receipts then accumulate from those participant stocks.

   Entry barrier is not a magic moat score. `Barrier_Asset_Units` is a
   transparent weighted sum of the technical base plus accumulated complements,
   integrations, receipts and nodes. A hypothetical entrant accumulates
   `Entrant_Equivalent_Assets` at an explicit replication throughput; the model
   reports remaining catch-up years and cost. All coefficients/weights/costs
   are caller-supplied because they are scenario assumptions until calibrated
   against external cohorts and real competitor delivery data.

   For an honest no-network counterfactual, build a second model with
   :developer-network-coefficient, :stack-network-coefficient and
   :node-demand-coefficient set to zero, then compare with
   `network-effect-summary`.

   Time unit is years. opts: :dt (default 0.05), :method (default :rk4)."
  [xmile-model-ns params & [{:keys [dt method] :or {dt 0.05 method :rk4}}]]
  (require-network-effect-params! params)
  (let [{:keys [model sim-specs aux flow stock add-variable]} xmile-model-ns
        p params
        add-aux (fn [m nm k] (add-variable m (aux nm (num-eqn (get p k)))))]
    (-> (model (:name p)
               {:xmile/sim-specs
                (sim-specs 0.0 (double (:sim-years p))
                           {:xmile/dt dt :xmile/method method
                            :xmile/time-units "years"})})
        ;; Observed initial assets/participants and named scenario parameters.
        (add-aux "Owned_Capability_Providers" :owned-capability-providers)
        (add-aux "Owned_Reusable_Components" :owned-reusable-components)
        (add-aux "Operator_Compute_Nodes" :operator-nodes)
        (add-aux "Developer_Market" :developer-market)
        (add-aux "Organization_Market" :organization-market)
        (add-aux "Node_Market" :node-market)
        (add-aux "External_Developer_Rate" :external-developer-rate)
        (add-aux "Developer_Network_Coefficient" :developer-network-coefficient)
        (add-aux "Provider_Productivity" :provider-productivity)
        (add-aux "Component_Productivity" :component-productivity)
        (add-aux "External_Organization_Rate" :external-organization-rate)
        (add-aux "Stack_Network_Coefficient" :stack-network-coefficient)
        (add-aux "External_Node_Rate" :external-node-rate)
        (add-aux "Node_Demand_Coefficient" :node-demand-coefficient)
        (add-aux "Receipts_Per_Organization" :receipts-per-organization)
        (add-aux "Receipts_Per_Node" :receipts-per-node)
        (add-aux "Provider_Scale" :provider-scale)
        (add-aux "Component_Scale" :component-scale)
        (add-aux "Receipt_Scale" :receipt-scale)
        (add-aux "Node_Scale" :node-scale)
        (add-aux "Complement_Value_Weight" :complement-value-weight)
        (add-aux "Technical_Base_Units" :technical-base-units)
        (add-aux "Provider_Barrier_Weight" :provider-barrier-weight)
        (add-aux "Component_Barrier_Weight" :component-barrier-weight)
        (add-aux "Receipt_Barrier_Weight" :receipt-barrier-weight)
        (add-aux "Organization_Barrier_Weight" :organization-barrier-weight)
        (add-aux "Node_Barrier_Weight" :node-barrier-weight)
        (add-aux "Entrant_Replication_Throughput" :entrant-replication-throughput)
        (add-aux "Entrant_Cost_Per_Unit" :entrant-cost-per-unit)
        (add-aux "Fixed_Technical_Cost" :fixed-technical-cost)

        ;; Stocks.
        (add-variable (stock "Independent_Developers"
                             (num-eqn (:initial-independent-developers p))
                             {:xmile/inflows #{"Developer_Adoption"}
                              :xmile/non-negative? true}))
        (add-variable (stock "Independent_Providers"
                             (num-eqn (:initial-independent-providers p))
                             {:xmile/inflows #{"Provider_Creation"}
                              :xmile/non-negative? true}))
        (add-variable (stock "Independent_Components"
                             (num-eqn (:initial-independent-components p))
                             {:xmile/inflows #{"Component_Creation"}
                              :xmile/non-negative? true}))
        (add-variable (stock "Active_Organizations"
                             (num-eqn (:initial-active-organizations p))
                             {:xmile/inflows #{"Organization_Adoption"}
                              :xmile/non-negative? true}))
        (add-variable (stock "Independent_Compute_Nodes"
                             (num-eqn (:initial-independent-nodes p))
                             {:xmile/inflows #{"Node_Onboarding"}
                              :xmile/non-negative? true}))
        (add-variable (stock "Verified_Receipts"
                             (num-eqn (:initial-verified-receipts p))
                             {:xmile/inflows #{"Receipt_Creation"}
                              :xmile/non-negative? true}))
        (add-variable (stock "Entrant_Equivalent_Assets" "0"
                             {:xmile/inflows #{"Entrant_Asset_Creation"}
                              :xmile/non-negative? true}))

        ;; Complements and reinforcing acquisition loops.
        (add-variable (aux "Total_Providers"
                           "Owned_Capability_Providers + Independent_Providers"))
        (add-variable (aux "Total_Components"
                           "Owned_Reusable_Components + Independent_Components"))
        (add-variable (aux "Total_Compute_Nodes"
                           "Operator_Compute_Nodes + Independent_Compute_Nodes"))
        (add-variable (aux "Kotoba_Complement_Index"
                           "MIN(1, (Total_Providers / Provider_Scale + Total_Components / Component_Scale + Verified_Receipts / Receipt_Scale) / 3)"))
        (add-variable (aux "Independent_Network_Complement_Index"
                           "MIN(1, (Independent_Providers / Provider_Scale + Independent_Components / Component_Scale + Verified_Receipts / Receipt_Scale) / 3)"))
        (add-variable (aux "Stack_Complement_Index"
                           "MIN(1, (Kotoba_Complement_Index + Total_Compute_Nodes / Node_Scale) / 2)"))
        (add-variable (flow "Developer_Adoption"
                            "MAX(0, (External_Developer_Rate + Developer_Network_Coefficient * Independent_Developers * Kotoba_Complement_Index) * (Developer_Market - Independent_Developers) / Developer_Market)"))
        (add-variable (flow "Provider_Creation"
                            "Provider_Productivity * Independent_Developers"))
        (add-variable (flow "Component_Creation"
                            "Component_Productivity * Independent_Developers"))
        (add-variable (flow "Organization_Adoption"
                            "MAX(0, (External_Organization_Rate + Stack_Network_Coefficient * Active_Organizations * Stack_Complement_Index) * (Organization_Market - Active_Organizations) / Organization_Market)"))
        (add-variable (flow "Node_Onboarding"
                            "MAX(0, (External_Node_Rate + Node_Demand_Coefficient * Active_Organizations) * (Node_Market - Independent_Compute_Nodes) / Node_Market)"))
        (add-variable (flow "Receipt_Creation"
                            "Receipts_Per_Organization * Active_Organizations + Receipts_Per_Node * Independent_Compute_Nodes"))

        ;; Value and marginal-network diagnostics.
        (add-variable (aux "Kotoba_Ecosystem_Value"
                           "Independent_Developers * (1 + Complement_Value_Weight * Kotoba_Complement_Index)"))
        (add-variable (aux "Kotoba_Ecosystem_Value_Multiplier"
                           "1 + Complement_Value_Weight * Kotoba_Complement_Index"))
        (add-variable (aux "Kotoba_Independent_Network_Value_Multiplier"
                           "1 + Complement_Value_Weight * Independent_Network_Complement_Index"))
        (add-variable (aux "Developer_Adoption_From_Network"
                           "MAX(0, Developer_Adoption - External_Developer_Rate * (Developer_Market - Independent_Developers) / Developer_Market)"))
        (add-variable (aux "Developer_Network_Adoption_Share"
                           "Developer_Adoption_From_Network / MAX(Developer_Adoption, 0.000001)"))
        (add-variable (aux "Organization_Adoption_From_Network"
                           "MAX(0, Organization_Adoption - External_Organization_Rate * (Organization_Market - Active_Organizations) / Organization_Market)"))
        (add-variable (aux "Organization_Network_Adoption_Share"
                           "Organization_Adoption_From_Network / MAX(Organization_Adoption, 0.000001)"))

        ;; Replication barrier and a moving entrant catch-up target.
        (add-variable (aux "Barrier_Asset_Units"
                           "Technical_Base_Units + Provider_Barrier_Weight * Total_Providers + Component_Barrier_Weight * Total_Components + Receipt_Barrier_Weight * Verified_Receipts + Organization_Barrier_Weight * Active_Organizations + Node_Barrier_Weight * Total_Compute_Nodes"))
        (add-variable (aux "Entrant_Remaining_Units"
                           "MAX(0, Barrier_Asset_Units - Entrant_Equivalent_Assets)"))
        (add-variable (flow "Entrant_Asset_Creation"
                            "MIN(Entrant_Replication_Throughput, Entrant_Remaining_Units)"))
        (add-variable (aux "Entrant_Catchup_Years"
                           "Entrant_Remaining_Units / Entrant_Replication_Throughput"))
        (add-variable (aux "Entrant_Catchup_Cost"
                           "Fixed_Technical_Cost + Entrant_Remaining_Units * Entrant_Cost_Per_Unit"))
        (add-variable (aux "New_Entrant_Catchup_Years"
                           "Barrier_Asset_Units / Entrant_Replication_Throughput"))
        (add-variable (aux "New_Entrant_Catchup_Cost"
                           "Fixed_Technical_Cost + Barrier_Asset_Units * Entrant_Cost_Per_Unit"))
        (add-variable (aux "Entry_Barrier_Index"
                           "Barrier_Asset_Units / MAX(Technical_Base_Units, 0.000001)")))))

(defn- nearest-series-value [result variable t]
  (let [times (:xmile/times result)
        series (get-in result [:xmile/series variable])
        nearest (first (sort-by (fn [idx] (#?(:cljs js/Math.abs :clj Math/abs)
                                               (- (nth times idx) t)))
                                (range (count times))))]
    (nth series nearest)))

(defn network-effect-summary
  "Compare a network-enabled model with a zero-feedback counterfactual.
   Returns checkpoint values, not claims about causality: causal attribution
   still depends on the caller eventually calibrating the supplied scenario
   coefficients from independent cohorts."
  [execute-run network-model no-network-model checkpoints]
  (let [network (execute-run network-model)
        baseline (execute-run no-network-model)]
    {:checkpoints
     (into (sorted-map)
           (for [t checkpoints]
             (let [n-dev (nearest-series-value network "Independent_Developers" t)
                   b-dev (nearest-series-value baseline "Independent_Developers" t)
                   n-org (nearest-series-value network "Active_Organizations" t)
                   b-org (nearest-series-value baseline "Active_Organizations" t)]
               [t {:independent-developers n-dev
                   :developers-without-network b-dev
                   :developer-uplift (- n-dev b-dev)
                   :developer-uplift-ratio (if (pos? b-dev) (/ n-dev b-dev) nil)
                   :active-organizations n-org
                   :organizations-without-network b-org
                   :organization-uplift (- n-org b-org)
                   :organization-uplift-ratio (if (pos? b-org) (/ n-org b-org) nil)
                   :independent-compute-nodes
                   (nearest-series-value network "Independent_Compute_Nodes" t)
                   :verified-receipts
                   (nearest-series-value network "Verified_Receipts" t)
                   :kotoba-ecosystem-value-multiplier
                   (nearest-series-value network "Kotoba_Ecosystem_Value_Multiplier" t)
                   :kotoba-independent-network-value-multiplier
                   (nearest-series-value network "Kotoba_Independent_Network_Value_Multiplier" t)
                   :developer-network-adoption-share
                   (nearest-series-value network "Developer_Network_Adoption_Share" t)
                   :organization-network-adoption-share
                   (nearest-series-value network "Organization_Network_Adoption_Share" t)
                   :entry-barrier-index
                   (nearest-series-value network "Entry_Barrier_Index" t)
                   :barrier-asset-units
                   (nearest-series-value network "Barrier_Asset_Units" t)
                   :entrant-catchup-years
                   (nearest-series-value network "Entrant_Catchup_Years" t)
                   :entrant-catchup-cost
                   (nearest-series-value network "Entrant_Catchup_Cost" t)
                   :new-entrant-catchup-years
                   (nearest-series-value network "New_Entrant_Catchup_Years" t)
                   :new-entrant-catchup-cost
                   (nearest-series-value network "New_Entrant_Catchup_Cost" t)}])))}))

(defn crossing-year
  "First simulated time at which the stock's series crosses `threshold` in
   the direction implied by `annual-rate`'s sign (rising through it if
   positive, falling through it if negative), or nil if it never does within
   the simulated horizon -- nil is a real finding (never crosses at this
   rate, within this horizon), not a missing value."
  [execute-run model annual-rate threshold]
  (let [result (execute-run model)
        times (:xmile/times result)
        series (get-in result [:xmile/series "Stock"])
        crossed? (if (neg? annual-rate) #(<= % threshold) #(>= % threshold))]
    (some (fn [[t v]] (when (crossed? v) t)) (map vector times series))))

(defn project
  "Run `model` (via the `execute-run` fn, i.e. xmile.execute/run from
   org-oasis-open-xmile) and return a small, honest summary rather than the
   full raw series: the stock's value at t=0 and at each checkpoint day in
   `checkpoint-days` (only checkpoints that actually fall within the
   simulated range are included -- no extrapolation past :xmile/stop)."
  [execute-run model checkpoint-days]
  (let [result (execute-run model)
        times (:xmile/times result)
        series (get-in result [:xmile/series "Stock"])
        stop (get-in model [:xmile/sim-specs :xmile/stop])
        idx-for-day (fn [day]
                      ;; times are evenly spaced by dt from 0; find the closest index
                      (let [dt (- (second times) (first times))]
                        (round (/ day dt))))]
    {:initial (first series)
     :checkpoints (into {}
                        (keep (fn [day]
                                (when (<= day stop)
                                  [day (nth series (idx-for-day day))]))
                              checkpoint-days))}))
