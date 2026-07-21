(ns dynamics.sysml
  "A generic Source -> Conversion -> Sink structural skeleton, real OMG
   SysML v2 (via kotoba-lang/org-omg-sysmlv2) -- the STRUCTURAL counterpart
   to `dynamics.xmile`'s DYNAMIC (stock/flow/simulated) model of the same
   kind of system. `dynamics.xmile/acquisition-model` answers 'how does the
   stock evolve over time'; this namespace answers 'what is this system
   actually made of, and what governance/design requirements is it
   accountable to' -- the same real acquisition-funnel loops this catalog
   keeps analyzing (etzhayyim's F2, or any of `loop-archetypes`) have BOTH
   a dynamic and a structural description, and conflating the two (e.g.
   writing a requirement as a comment in dynamics.core instead of a real,
   traceable SysML RequirementUsage) is exactly the kind of ad-hoc
   substitute this correction cycle exists to stop doing.

   Callers MUST require `org-omg-sysmlv2`'s own `sysml.model`/
   `sysml.validate` on their classpath and pass the needed fns in via
   `sysml-model-ns`, the same host-injects-dependencies pattern as
   `dynamics.xmile`."
  )

(defn acquisition-system
  "Build a real SysML v2 model: three PartDefinitions (Source, Conversion,
   Sink -- named via `source-name`/`conversion-name`/`sink-name`) nested
   inside a top-level System PartUsage, connected Source->Conversion->Sink
   via ConnectionUsages, with an optional RequirementDefinition/Usage
   attached (satisfied by the System) for each map in `requirements`
   ({:name ... :text ... :req-id ...})."
  [sysml-model-ns {:keys [system-name source-name conversion-name sink-name requirements]
                    :or {requirements []}}]
  (let [{:keys [model add-element part-definition part-usage nest
                connection-definition connection-usage
                requirement-definition requirement-usage with-subject
                satisfy-requirement-usage]}
        sysml-model-ns
        source-usage-name (str source-name "-usage")
        conversion-usage-name (str conversion-name "-usage")
        sink-usage-name (str sink-name "-usage")
        system-usage-name (str system-name "-usage")
        base
        (-> (model system-name)
            (add-element (part-definition system-name))
            (add-element (part-definition source-name))
            (add-element (part-definition conversion-name))
            (add-element (part-definition sink-name))
            (add-element (part-usage source-usage-name source-name))
            (add-element (part-usage conversion-usage-name conversion-name))
            (add-element (part-usage sink-usage-name sink-name))
            (add-element (-> (part-usage system-usage-name system-name)
                              (nest source-usage-name)
                              (nest conversion-usage-name)
                              (nest sink-usage-name)))
            (add-element (connection-definition (str system-name "-Flow")))
            (add-element (connection-usage (str source-name "-to-" conversion-name)
                                            (str system-name "-Flow")
                                            [source-usage-name conversion-usage-name]))
            (add-element (connection-usage (str conversion-name "-to-" sink-name)
                                            (str system-name "-Flow")
                                            [conversion-usage-name sink-usage-name])))]
    (reduce
     (fn [m {:keys [name text req-id]}]
       (let [def-name (str name "-def")
             usage-name (str name "-usage")]
         (-> m
             (add-element (requirement-definition def-name (cond-> {} text (assoc :sysml/text text)
                                                                    req-id (assoc :sysml/req-id req-id))))
             (add-element (-> (requirement-usage usage-name def-name)
                               (with-subject system-usage-name)))
             (add-element (satisfy-requirement-usage (str name "-satisfy") usage-name system-usage-name)))))
     base
     requirements)))
