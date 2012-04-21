(defproject ld23 "1.0.0-SNAPSHOT"
  :description "netguy204's entry into Ludum Dare 23. Tiny World"
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :dev-dependencies [[lein-cljsbuild "0.1.8"]]

  :cljsbuild
  {:builds
   [
    {:source-path "src-cljs"
     :compiler
     {:output-to "javascripts/main.js"
      :optimizations :whitespace
      :pretty-print true
      }
     :id "standard"
     }

    {:source-path "src-cljs"
     :compiler
     {:output-to "javascripts/compiled.js"
      :optimizations :advanced
      :pretty-print false
      :externs ["jukebox-externs.js"]
      }
     :id "advanced"
     }

    ]
   })
