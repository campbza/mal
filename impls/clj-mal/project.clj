(defproject clj-mal "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  ;; To run a step with correct readline behavior:
  ;;   lein trampoline with-profile stepX run
  ;; To generate a executable uberjar (in target/) for a step:
  ;;   lein with-profile stepX repl
  :profiles {:step0 {:main clj-mal.step0-repl
                     :uberjar-name "step0_repl.jar"
                     :aot [clj-mal.step0-repl]}
             :step1 {:main clj-mal.step1-read-print
                     :uberjar-name "step1_read_print.jar"
                     :aot [clj-mal.step1-read-print]}
             :step2 {:main clj-mal.step2-eval
                     :uberjar-name "step2_eval.jar"
                     :aot [clj-mal.step2-eval]}
             :step3 {:main clj-mal.step3-env
                     :uberjar-name "step3_env.jar"
                     :aot [clj-mal.step3-env]}})
