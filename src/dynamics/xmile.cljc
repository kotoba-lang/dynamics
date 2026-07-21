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
