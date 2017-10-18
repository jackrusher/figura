(defproject figura "0.1.0-SNAPSHOT"
  :description "A library/tool for livecoding printable shapes in clojure."
  :url "http://github.com/jackrusher/figura"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.443"]
                 [rovanion/geom "0.0.1194"]
                 [org.jogamp.gluegen/gluegen-rt "2.3.2" :classifier "natives-macosx-universal"]
                 [org.jogamp.jogl/jogl-all "2.3.2" :classifier "natives-macosx-universal"]])
