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

## Test

```bash
nbb --classpath "src:test" test/run_tests.cljs
```

## License

MIT.
