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
                             keyboard-velocity-generator until-false
                             jump-velocity-generator ground-friction-generator
                             ]))
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.string.format :as format)
            (goog.events :as gevents)
            (goog.Timer :as timer)
            (goog.events.KeyHandler :as geventskey)
            (clojure.browser.event :as event)
            (clojure.browser.repl :as repl)))


(def *current-map* (atom nil))
(def *symbols* nil)

(def *orange-font* nil)
(def *font-chars* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\"?!./")

(def *backdrop* (get-img (str "graphics/backdrop.png?" (Math/random))))
(def *player-sprite* nil)


(defn setup-symbols [sprites]
  (let [dims showoff.showoff.*tile-in-world-dims*]
    (set!
     *symbols*
     {[255 255 255]
      {:kind :skip}
      
      [0 0 0]
      {:kind :image
       :image (resize-nearest-neighbor sprites [0 16 16 16] dims)
       :collidable true
       :shape :rect}

      [0 255 0]
      {:kind :image
       :image (resize-nearest-neighbor sprites [0 32 16 16] dims)
       :collidable true
       :shape :rect}

      [102 102 102]
      {:kind :image
       :image (resize-nearest-neighbor sprites [16 32 16 16] dims)
       :collidable true
       :shape :rect}
      
      })

    (set! *player-sprite*
          {:left (resize-nearest-neighbor sprites [0 0 16 16] dims)
           :right (resize-nearest-neighbor sprites [16 0 16 16] dims)})))

(defn with-prepared-assets [callback]
  (with-loaded-font "graphics/basic-font.gif" *font-chars* [8 8] 2 (color [196 106 59])
    (fn [font]
      (set! *orange-font* font)))

  (with-img "graphics/sprites.png"
    (fn [sprites]
      (setup-symbols sprites)
      (with-img (str "graphics/world.gif?" (Math/random))
        (fn [map-img]
          (reset! *current-map* (load-map map-img *symbols*))
          (callback))))))


(defn game-loop []
  (cycle-once draw-world)
  true)

(defn draw-world []
  (clear)
  (let [ctx (context)
        [vx vy _ _] (viewport-rect)]
    (.drawImage ctx *backdrop* (- (* vx 9)) (- (* vy 9)))
    (draw-map @*current-map*)
    (draw-entities)))

(defn input-state []
  showoff.core.*command-state-map*)

(defrecord Player [particle]
  showoff.showoff.Rectable
  (to-rect [player]
    (let [[x y] (:position @particle)]
      [(+ x 0.2) (+ y 0.1) 0.6 0.9]))

  showoff.showoff.Tickable
  (tick [player]
    ;; record the last directon of motion so we can draw our sprite
    (cond
     ((input-state) (.-LEFT gevents/KeyCodes))
     (swap! particle conj {:direction :left})

     ((input-state) (.-RIGHT gevents/KeyCodes))
     (swap! particle conj {:direction :right}))
    
    ;; motion and collision detectin
    (reset! particle (apply-particle-vs-map (integrate-particle @particle)
                                            @*current-map*
                                            (to-rect player)
                                            0.0))) ;; restitution

  showoff.showoff.Drawable
  (draw [player ctx]
    (let [sprite-key (or (:direction @particle) :right)
          sprite (sprite-key *player-sprite*)]
      (draw-sprite ctx sprite (:position @particle)))))

;;; the particle provides keyboard interaction, jumping, etc
(def *player-speed* 40)
(def *player*
  (Player.
   (atom
    {:mass 5
     :position [30 2]
     :velocity [0 0]
    
     ;; bring to a stop quickly
     :force-generators
     [(drag-force-generator 2.0)
      (ground-friction-generator *current-map* #(to-rect *player*) 30)
      (gravity-force-generator 16)
      (keyboard-velocity-generator
       (.-LEFT gevents/KeyCodes) [(- *player-speed*) 0])
      (keyboard-velocity-generator
       (.-RIGHT gevents/KeyCodes) [*player-speed* 0])
      (jump-velocity-generator *current-map* #(to-rect *player*) 400 0.5)
      ]
     })))

(def +viewport-spring-constant+ 50)
(def +viewport-drag-coefficient+ 3)
(def +viewport-max-displacement+ 2)

(def *viewport*
  (Viewport.
   [16 10]
   (atom
    {:mass 1
     :position [30 2]
     :velocity [0 0]
     
     ;; try to keep the player character basically centered
     :force-generators
     [(fn [p] (spring-force (vec-sub (:position @(:particle *player*))
                                     (vec-sub (rect-center (viewport-rect))
                                              [0 1]))
                            +viewport-max-displacement+
                            +viewport-spring-constant+))
      (drag-force-generator +viewport-drag-coefficient+)]})))

(defn ^:export game []
  (let [screen-size [640 480]
        canvas (make-canvas screen-size)]
    
    (dom/appendChild (content) canvas)
    (set-display-and-viewport canvas screen-size #(to-rect *viewport*))
    (prepare-input)
    (add-entity @*current-map* *viewport*)
    (add-entity @*current-map* *player*)
    (with-prepared-assets #(until-false game-loop))))





(defn ^:export map-viewer []
  (dom/setTextContent (content) "")

  (let [screen-size [640 480]
        canvas (make-canvas screen-size)
        viewport-speed (* 30 showoff.showoff.+secs-per-tick+)
        viewport (Viewport.
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
                      (.-DOWN gevents/KeyCodes) [0 viewport-speed])]}))]
    
    (dom/appendChild (content) canvas)
    (set-display-and-viewport canvas screen-size #(to-rect viewport))
    (prepare-input)
    (add-entity {} viewport)
    (with-prepared-assets #(until-false game-loop))))



