(ns generate-kotoba-network-effect
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [dynamics.xmile :as dx]
            [xmile.execute :as execute]
            [xmile.model :as model]
            [xmile.validate :as validate]
            [xmile.xml :as xml]))

(def xmile-model-ns
  {:model model/model
   :sim-specs model/sim-specs
   :aux model/aux
   :flow model/flow
   :stock model/stock
   :add-variable model/add-variable})

(defn- read-edn [path]
  (edn/read-string (slurp path)))

(defn- write-edn! [path value]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (binding [*out* w]
      (pprint/pprint value))))

(defn- sha256-file [path]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream path)]
      (let [buffer (byte-array 8192)]
        (loop []
          (let [n (.read in buffer)]
            (when (pos? n)
              (.update digest buffer 0 n)
              (recur))))))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest digest)))))

(defn- no-network-params [params]
  (-> params
      (assoc :name (str (:name params) "_No_Network_Counterfactual"))
      (assoc :developer-network-coefficient 0
             :stack-network-coefficient 0
             :node-demand-coefficient 0)))

(defn generate!
  ([] (generate! "examples/kotoba-network-effect-scenarios.edn"
                 "examples/generated/kotoba-network-effect-scenarios.xmile"
                 "examples/generated/kotoba-network-effect-results.edn"
                 "examples/generated/manifest.edn"))
  ([input-path xmile-path results-path manifest-path]
   (let [{:model/keys [as-of classification observed-evidence]
          :keys [common scenarios checkpoints-years]} (read-edn input-path)
         built (into (sorted-map)
                     (for [[scenario assumptions] scenarios
                           :let [params (merge common assumptions)
                                 network (dx/network-effect-barrier-model
                                          xmile-model-ns params)
                                 baseline (dx/network-effect-barrier-model
                                           xmile-model-ns (no-network-params params))]]
                       [scenario {:params params
                                  :network network
                                  :baseline baseline}]))
         doc {:xmile/header
              {:xmile/vendor "kotoba-lang/dynamics"
               :xmile/product {:xmile/name "Kotoba network-effect and entry-barrier scenarios"
                               :xmile/version "1.0"}
               :xmile/name "Kotoba stack network scenarios"}
              :xmile/models
              (vec (mapcat (fn [[_ {:keys [network baseline]}]]
                             [network baseline])
                           built))}
         problems (validate/validate-doc doc)]
     (when-not (validate/valid? problems)
       (throw (ex-info "generated Kotoba network-effect XMILE is invalid"
                       {:problems problems})))
     (io/make-parents xmile-path)
     (spit xmile-path (xml/emit-string doc))
     (write-edn!
      results-path
      {:model/id :kotoba-stack-network-effect-and-entry-barrier
       :model/as-of as-of
       :model/classification classification
       :model/interpretation
       "Observed starting stocks plus conservative/base/upside sensitivity assumptions. These are scenarios, not forecasts or measured causal coefficients. Entry-barrier indexes are comparable over time within one scenario; scenario weights differ, so compare the explicit catch-up ranges rather than ranking indexes across scenarios."
       :model/observed-evidence observed-evidence
       :model/checkpoints-years checkpoints-years
       :model/scenarios
       (into (sorted-map)
             (for [[scenario {:keys [params network baseline]}] built]
               [scenario
                {:assumptions (apply dissoc params
                                     [:initial-independent-developers
                                      :owned-capability-providers
                                      :initial-independent-providers
                                      :owned-reusable-components
                                      :initial-independent-components
                                      :initial-active-organizations
                                      :initial-independent-nodes
                                      :operator-nodes
                                      :initial-verified-receipts])
                 :result (dx/network-effect-summary execute/run network baseline
                                                    checkpoints-years)}]))})
     (write-edn!
      manifest-path
      {:generated/by "examples/generate_kotoba_network_effect.clj"
       :generated/source input-path
       :generated/source-sha256 (sha256-file input-path)
       :generated/artifacts
       [{:path xmile-path :sha256 (sha256-file xmile-path)}
        {:path results-path :sha256 (sha256-file results-path)}]})
     {:xmile xmile-path
      :results results-path
      :manifest manifest-path
      :models (count (:xmile/models doc))
      :validation-problems (count problems)})))

(defn -main [& _]
  (prn (generate!)))
