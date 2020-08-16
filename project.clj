(defproject wfclj "0.3.0"
  :description "Toy implementation of the wave function collapse algorithm inspired by this python version: https://github.com/robert/wavefunction-collapse Authorative information regarding the algo is availale here: https://gridbugs.org/wave-function-collapse/#:~:text=Wave%20Function%20Collapse%20is%20a,frequently%20each%20tile%20should%20appear."
  :url "http://example.com/FIXME"
  :license {:name "APACHE LICENSE, VERSION 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clojure-term-colors "0.1.0"]
                 [clansi "1.0.0"]]
  :repl-options {:init-ns wfclj.core})
