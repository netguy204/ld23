(ns ld23.core
  (:use (showoff.showoff :only [get-img remove-entity add-entity supported-by-map
                                Rectable Tickable Drawable
                                vec-add vec-dot vec-scale vec-sub vec-unit
                                rect-center viewport-rect clear display
                                tick-entities tick to-rect draw
                                drag-force-generator gravity-force-generator
                                integrate-particle spring-force
                                apply-particle-vs-map load-map draw-map
                                draw-sprite make-canvas get-img with-img
                                draw-entities img-dims context filled-rect
                                color map-collisions rect->idxs idx->coords
                                resize-nearest-neighbor record-vs-rect
                                set-display-and-viewport cycle-once
                                head-bumped-map with-loaded-font draw-text
                                draw-text-centered stats-string])
        (showoff.core :only [content clj->js Viewport prepare-input
                             keyboard-velocity-generator until-false]))
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)
            (goog.events :as gevents)
            (goog.Timer :as timer)
            (goog.events.KeyHandler :as geventskey)
            (clojure.browser.event :as event)
            (clojure.browser.repl :as repl)))


(def *command-state-map* #{})
(def *current-map* nil)
(def *symbols* nil)

(def *orange-font* nil)
(def *font-chars* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\"?!./")

;;; for now, just a panning viewport. making sure everything is still
;;; working like I expect it to be
(def viewport-speed (* 5 showoff.showoff.+secs-per-tick+))
(def *viewport*
  (Viewport.
   [16 10]
   (atom
    {:mass 1
     :position [0 0]
     :velocity [0 0]

     :offset-generators
     [(keyboard-velocity-generator
       (.-LEFT gevents/KeyCodes) [(- viewport-speed) 0])
      (keyboard-velocity-generator
       (.-RIGHT gevents/KeyCodes) [viewport-speed 0])
      (keyboard-velocity-generator
       (.-UP gevents/KeyCodes) [0 (- viewport-speed)])
      (keyboard-velocity-generator
       (.-DOWN gevents/KeyCodes) [0 viewport-speed])]})))


(def *sprite-dims* [32 32])
(def *backdrop* (get-img "graphics/backdrop.png"))

(defn setup-symbols [sprites]
  (set!
   *symbols*
   {[255 255 255]
    {:kind :skip}

    [0 0 0]
    {:kind :image
     :image (resize-nearest-neighbor sprites [0 16 16 16] *sprite-dims*)
     :collidable true
     :shape :rect}

    }))

(defn with-prepared-assets [callback]
  (with-loaded-font "graphics/basic-font.gif" *font-chars* [8 8] 2 (color [196 106 59])
    (fn [font]
      (set! *orange-font* font)))

  (with-img "graphics/sprites.png"
    (fn [sprites]
      (setup-symbols sprites)
      (with-img "graphics/world.gif"
        (fn [map-img]
          (set! *current-map* (load-map map-img *symbols*))
          (callback))))))


(defn game-loop []
  (cycle-once draw-world)
  true)

(defn draw-world []
  (clear)
  (let [ctx (context)]
    (.drawImage ctx *backdrop* 0 0)
    (draw-map *current-map*)
    (draw-text-centered ctx *orange-font* "LD 23!" [400 300]))  )

(defn ^:export main []
  (dom/setTextContent (content) "")

  (let [screen-size [640 480]
        canvas (make-canvas screen-size)]
    (dom/appendChild (content) canvas)
    (set-display-and-viewport canvas screen-size #(to-rect *viewport*))
    (prepare-input)
    (add-entity {} *viewport*)
    (with-prepared-assets #(until-false game-loop))))



