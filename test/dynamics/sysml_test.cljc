(ns dynamics.sysml-test
  (:require [clojure.test :refer [deftest is testing]]
            [dynamics.sysml :as ds]
            [sysml.model :as sm]
            [sysml.validate :as validate]))

(def sysml-ns
  {:model sm/model :add-element sm/add-element :part-definition sm/part-definition
   :part-usage sm/part-usage :nest sm/nest
   :connection-definition sm/connection-definition :connection-usage sm/connection-usage
   :requirement-definition sm/requirement-definition :requirement-usage sm/requirement-usage
   :with-subject sm/with-subject :satisfy-requirement-usage sm/satisfy-requirement-usage})

(deftest acquisition-system-builds-a-valid-real-sysml-model-test
  (testing "the generic acquisition-system, with no requirements, is a structurally valid SysML v2 model"
    (let [built (ds/acquisition-system sysml-ns {:system-name "Test" :source-name "S" :conversion-name "C" :sink-name "K"})]
      (is (validate/valid? (validate/validate built))))))

(deftest acquisition-system-nests-all-three-parts-under-the-system-test
  (testing "Source/Conversion/Sink usages are all real nested members of the top-level System usage"
    (let [built (ds/acquisition-system sysml-ns {:system-name "Test" :source-name "S" :conversion-name "C" :sink-name "K"})]
      (is (= #{"S-usage" "C-usage" "K-usage"} (sm/all-nested-usages built "Test-usage"))))))

(deftest acquisition-system-with-requirements-stays-valid-and-traceable-test
  (testing "attaching a requirement produces a real RequirementUsage satisfied by the system, and stays valid"
    (let [built (ds/acquisition-system sysml-ns
                                        {:system-name "Test" :source-name "S" :conversion-name "C" :sink-name "K"
                                         :requirements [{:name "NoLockIn" :text "anti-monopoly" :req-id "X-1"}]})]
      (is (validate/valid? (validate/validate built)))
      (is (sm/requirement-usage? (sm/lookup built "NoLockIn-usage")))
      (is (= "X-1" (:sysml/req-id (sm/lookup built "NoLockIn-def")))))))

(deftest etzhayyim-acquisition-system-real-charter-requirements-test
  (testing "the actual etzhayyim structural model this cycle built, with real Charter-cited requirements, validates and round-trips the citations"
    (let [built (ds/acquisition-system sysml-ns
                                        {:system-name "EtzhayyimAdherentAcquisition"
                                         :source-name "Website" :conversion-name "DIDSBTRitual" :sink-name "Adherent"
                                         :requirements
                                         [{:name "NoStateRegistration"
                                           :text "NOT registered under Japan Religious Corporations Act (宗教法人法) -- Preamble section 0.4, Lv7+ unanimity lock"
                                           :req-id "CHARTER-0.4"}
                                          {:name "AntiMonopoly"
                                           :text "Anti-monopoly / anti-dependence"
                                           :req-id "CHARTER-1.12"}]})]
      (is (validate/valid? (validate/validate built)))
      (is (= #{"Website-usage" "DIDSBTRitual-usage" "Adherent-usage"}
             (sm/all-nested-usages built "EtzhayyimAdherentAcquisition-usage")))
      (is (= "CHARTER-0.4" (:sysml/req-id (sm/lookup built "NoStateRegistration-def"))))
      (is (= "CHARTER-1.12" (:sysml/req-id (sm/lookup built "AntiMonopoly-def")))))))

(deftest fleet-model-builds-a-valid-real-sysml-model-test
  (testing "N member PartUsages, all nested under the Fleet usage, is a structurally valid model -- distinct shape from acquisition-system's fixed 3 roles"
    (let [built (ds/fleet-model sysml-ns {:fleet-name "Fleet" :member-definition-name "Member"
                                           :members [{:name "a"} {:name "b"} {:name "c"}]})]
      (is (validate/valid? (validate/validate built)))
      (is (= #{"a-usage" "b-usage" "c-usage"} (sm/all-nested-usages built "Fleet-usage"))))))

(deftest add-fleet-requirement-traces-per-member-satisfaction-test
  (testing "each member gets its OWN RequirementUsage; satisfy is added only where satisfied? is true, never fabricated"
    (let [fleet (ds/fleet-model sysml-ns {:fleet-name "Fleet" :member-definition-name "Member"
                                           :members [{:name "a"} {:name "b"}]})
          built (ds/add-fleet-requirement sysml-ns fleet
                                           {:name "MustBeRegistered" :text "real requirement" :req-id "R-1"
                                            :members [{:name "a" :satisfied? true} {:name "b" :satisfied? false}]})]
      (is (validate/valid? (validate/validate built)))
      (is (sm/requirement-usage? (sm/lookup built "a--MustBeRegistered-usage")))
      (is (sm/requirement-usage? (sm/lookup built "b--MustBeRegistered-usage")))
      (is (some? (sm/lookup built "a--MustBeRegistered-satisfy")))
      (is (nil? (sm/lookup built "b--MustBeRegistered-satisfy"))))))

(deftest add-fleet-requirement-supports-a-real-subset-of-members-test
  (testing "a requirement not applicable to every member simply omits those members -- no RequirementUsage at all for them, distinct from an unsatisfied one"
    (let [fleet (ds/fleet-model sysml-ns {:fleet-name "Fleet" :member-definition-name "Member"
                                           :members [{:name "a"} {:name "b"}]})
          built (ds/add-fleet-requirement sysml-ns fleet
                                           {:name "OnlyAppliesToA" :members [{:name "a" :satisfied? true}]})]
      (is (validate/valid? (validate/validate built)))
      (is (sm/requirement-usage? (sm/lookup built "a--OnlyAppliesToA-usage")))
      (is (nil? (sm/lookup built "b--OnlyAppliesToA-usage"))))))
