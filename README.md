# dynamics

Stock-flow-loop system dynamics primitives and Donella Meadows leverage-point
scoring, as a portable `.cljc` library. No orchestration prefix (per
`kotoba-lang/loop-ux-kaizen`'s `resources/repository-rules.edn` taxonomy): this
repository owns the domain math only, the same way `design-quality` or
`hinshitsu` own their domains. The continuous observe/evaluate/decide/act
orchestrator that runs this math against real entity data lives in
[`kotoba-lang/loop-system-dynamics`](https://github.com/kotoba-lang/loop-system-dynamics).

## Why this exists

`ADR-2607203000` (com-junkawasaki/root) needed to answer two related
questions with real numbers instead of vibes:

1. Where in a growth loop's stock-flow structure does an intervention have
   outsized leverage, per Meadows' 1999 "Leverage Points" hierarchy?
2. Structurally -- not morally -- why do extractive systems (surveillance
   ad-tech, speculative derivatives, aggressive recruitment) so often
   out-compete value-aligned commons/mutual-aid systems?

Both questions turn out to need the same primitives: stocks, flows, loops,
and a transparent, auditable scoring formula over them.

## Design principle: no entity is categorically out of scope

The schema admits any organization or system as an entity given the minimum
stock+flow facts. What is finite at any point in time is which entities have
been fed real, dated, sourced data -- not which entities the model can
represent. Coverage grows as more entities are observed; it is never capped
by policy. See `ADR-2607203000` for the full statement of this as a
repo-wide rule, not just a design note here.

"Computed" always means instantiated against real facts. This library never
fabricates a number and presents it as measured -- see
`dynamics.core/loop-structural-strength`, which returns `nil` rather than a
guessed value when a loop's cycle time has never actually been observed.

## Usage

```clojure
(require '[dynamics.core :as d])

(d/leverage-score {:id :reframe-goal :band :band/A :tractability 0.8})
;; => {:id :reframe-goal, :band :band/A, :tractability 0.8, :base-score 8.0}

(d/compare-archetypes)
;; => {:ranked [[:speculative-crypto-derivatives 5.2e8] ...]
;;     :unmeasured [:etzhayyim-adherent-loop]}
```

## Real simulation and structural modeling: `dynamics.xmile` / `dynamics.sysml`

**Correction (2026-07-21):** `kotoba-lang/org-oasis-open-xmile` (OASIS XMILE
1.0, a real, ADR-authoritative system-dynamics engine with an actual
Euler/RK4 simulator) already existed, and was already the designated
computational substrate for system dynamics in kotoba-lang (`ADR-2607072350`,
2026-07-07), when this repository's own `stock`/`flow`/`loop*`/
`loop-structural-strength` were built two weeks later (`ADR-2607203000`,
2026-07-20) without checking for it first -- exactly the "did you check for
existing infrastructure before building new" failure mode this workspace's
own CLAUDE.md repeatedly warns about elsewhere (BMC/Lean Loop tracking,
design-quality scoring, coscientist loops). `loop-structural-strength`
itself stays (it is a genuinely different, cheaper question -- comparative
ranking from 4 coarse parameters, no full equation model needed per
archetype), but any caller who wants an actual projected TRAJECTORY over
time, not a single comparative score, should use `dynamics.xmile` instead of
inventing another one-off simulator.

- **`dynamics.xmile`** -- `acquisition-model` builds a real XMILE stock/flow
  model (a constant real inflow rate feeding a stock through a constant
  real conversion rate) that `xmile.execute/run` (from
  `kotoba-lang/org-oasis-open-xmile`) actually simulates; `project` returns
  a small summary at chosen checkpoint days.
- **`dynamics.sysml`** -- `acquisition-system` builds the STRUCTURAL
  counterpart, real OMG SysML v2 (via `kotoba-lang/org-omg-sysmlv2`):
  Source/Conversion/Sink parts wired by real connections, with real,
  traceable `RequirementUsage`s (e.g. citing an actual Charter clause) that
  `sysml.validate` checks structurally.

Both are thin, honest convenience layers: they take the needed builder fns
from the real standard library as a map argument (`xmile-model-ns`/
`sysml-model-ns`), rather than re-implementing or hard-depending on them,
so this repo's own `deps.edn` stays dependency-free and callers wire the
real libraries in via `--classpath` (see CI workflow for the exact sibling
checkout + classpath shape).

```clojure
(require '[dynamics.xmile :as dx] '[xmile.model :as m] '[xmile.execute :as execute])

(def xmile-ns {:model m/model :sim-specs m/sim-specs :aux m/aux :flow m/flow
               :stock m/stock :add-variable m/add-variable})

(def projection
  (dx/project execute/run
              (dx/acquisition-model xmile-ns {:name "acq" :inflow-rate 264 :conversion-rate 0.000178
                                               :initial-stock 1 :sim-days 3650})
              [365 1825 3650]))
;; => {:initial 1, :checkpoints {365 18.2, 1825 87.0, 3650 172.8}}
```

`dynamics.xmile` also has `percentage-rate-model`, the complementary shape:
not a constant ADDITIVE inflow (`acquisition-model`), but a constant
PROPORTIONAL/exponential rate applied to the stock itself (`Stock' = Stock
* Annual_Rate`, rate may be negative) -- the right shape for a real,
already-measured year-over-year percentage change on a stock/level, as
opposed to a flow feeding an accumulator. Using the wrong shape for a given
real fact is a modeling error even when both "run". Note this integrates
the CONTINUOUS exponential closed form (`S0 * e^(rt)`), not discrete annual
compounding (`S0 * (1+r)^t`) -- the two diverge slightly (e.g. 10 years at
-4.94%/yr: continuous gives 14.10, discrete compounding gives 13.92) and
callers should be aware of which one they're getting.

```clojure
;; real 2025->2026 ACA marketplace enrollment: 24.3M -> 23.1M, -4.938%/yr
(def rate (- (/ 23.1 24.3) 1))
(def decline (dx/percentage-rate-model xmile-ns {:name "aca" :initial-stock 23.1 :annual-rate rate :sim-years 30}))
(dx/project execute/run decline [10])           ;; => {:checkpoints {10 14.1}} (millions)
(dx/crossing-year execute/run decline rate 12.15) ;; => 13.1 (years to fall below half the 2025 peak)
```

**`dynamics.sysml` also has a second, distinct generic shape**: `fleet-model` +
`add-fleet-requirement`, for a real population of N same-kind members
(rather than `acquisition-system`'s fixed 3 roles) that need per-member
compliance tracing -- e.g. cloud-itonami's 797 per-ISIC/ISCO-code blueprint
repos, each individually either registered in `com-junkawasaki/root`'s
`manifest/west.yml` or not. `fleet-model` builds one PartDefinition with N
PartUsage members nested under a Fleet usage; `add-fleet-requirement`
attaches one shared RequirementDefinition with a per-member RequirementUsage
(subject = that member), adding a SatisfyRequirementUsage only where the
caller's real data says `:satisfied?` is true -- and accepts a real SUBSET
of members for a requirement that legitimately does not apply to all of
them (omitted members get no RequirementUsage at all for that requirement,
keeping "not applicable" structurally distinct from "measured and failing"):

```clojure
(require '[dynamics.sysml :as ds] '[sysml.model :as sm])

(def fleet
  (ds/fleet-model sysml-ns {:fleet-name "CloudItonamiCodes" :member-definition-name "ClassificationBlueprint"
                             :members [{:name "cloud-itonami-isic-6419"} {:name "cloud-itonami-isco-1321"}]}))

(def traced
  (ds/add-fleet-requirement sysml-ns fleet
                             {:name "RegisteredInWorkspace" :req-id "WEST-REG"
                              :members [{:name "cloud-itonami-isic-6419" :satisfied? true}
                                        {:name "cloud-itonami-isco-1321" :satisfied? false}]}))
```

## Test

```bash
# needs org-oasis-open-xmile, org-omg-sysmlv2, and dsl-core checked out as
# siblings (see .github/workflows/ci.yml for the exact pinned refs)
nbb --classpath "src:test:../org-oasis-open-xmile/src:../org-omg-sysmlv2/src:../dsl-core/src" test/run_tests.cljs
```

## License

MIT.
