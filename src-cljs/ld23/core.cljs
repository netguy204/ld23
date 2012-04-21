(ns ld23.core
  (:use (showoff.showoff :only [get-img remove-entity add-entity supported-by-map
                                Rectable Tickable Drawable
                                vec-add vec-dot vec-scale vec-sub vec-unit
                                rect-center viewport-rect clear display
                                tick-entities tick to-rect draw rect-intersect
                                drag-force-generator gravity-force-generator
                                integrate-particle spring-force
                                apply-particle-vs-map load-map draw-map
                                draw-sprite make-canvas get-img with-img
                                draw-entities img-dims context filled-rect
                                color map-collisions rect->idxs idx->coords
                                resize-nearest-neighbor record-vs-rect
                                set-display-and-viewport cycle-once
                                head-bumped-map with-loaded-font draw-text
                                draw-text-centered stats-string get-map-idx
                                set-map-idx coords->idx])
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
(def *hud* nil)
(def *collectables* nil)
(def *bag* (atom []))
(def *media-player* nil)

(defn play-sound [key]
  (.play *media-player* (name key)))

(defprotocol Collectable
  (collect [c]))

(defprotocol Useable
  (use-thing [obj user]))

(defprotocol Iconic
  (icon [obj]))

(defn posrec->rect [position rec]
  (let [[px py] position
        [w h] (:dims rec)]
    [px py w h]))

(defn cooldown-tick [obj]
  (let [cooldown (or (:cooldown @obj) 0)]
    (swap! obj conj {:cooldown (- cooldown showoff.showoff.+secs-per-tick+)})))

(defn is-cool? [obj]
  (<= (or (:cooldown @obj) 0) 0))

(defn cooldown-start [obj max-cooldown]
  (swap! obj conj {:cooldown max-cooldown}))

(defrecord StaticCollectable [position rec]
  showoff.showoff.Rectable
  (to-rect [c] (posrec->rect position rec))

  showoff.showoff.Drawable
  (draw [c ctx] (draw-sprite ctx (:image rec) position))

  Collectable
  (collect [c]
    (remove-entity @*current-map* c)))

(defn breakable-underneath? [rect]
  (let [[x y w h] rect
        test-rect [(+ x 0.1) (+ y h (- 0.1)) (- w 0.2) 0.3]
        hit-idx (first (map-collisions @*current-map* test-rect))]
    (when hit-idx
      (let [hit (get-map-idx @*current-map* hit-idx)]
        (if (:breakable hit)
          hit-idx)))))

;;; bag (we need to be able to tick this)
(defrecord Bag [contents]
  showoff.showoff.Tickable
  (tick [b] (doseq [item @contents]
              (tick item))))

(defn add-to-bag [item]
  (swap! *bag* conj item))

(defn remove-from-bag [item]
  (reset! *bag* (into [] (filter #(not (= item %)) @*bag*))))

(defrecord Jackhammer [position rec max-cooldown state]
  showoff.showoff.Rectable
  (to-rect [c] (posrec->rect position rec))

  showoff.showoff.Drawable
  (draw [c ctx] (draw-sprite ctx (:image rec) position))

  showoff.showoff.Tickable
  (tick [c] (cooldown-tick state))

  Iconic
  (icon [c] (:image rec))
  
  Collectable
  (collect [c]
    (remove-entity @*current-map* c)
    (add-to-bag c))

  Useable
  (use-thing [c user]
    (when (is-cool? state)
      (if-let [idx (breakable-underneath? (to-rect user))]
        (let [map-rec (get-map-idx @*current-map* idx)]
          (set-map-idx @*current-map* idx {:kind :skip})
          (add-entity @*current-map* (add-collectable :rubble (idx->coords @*current-map* idx) map-rec))
          (cooldown-start state max-cooldown))))))

(defrecord Rubble [position rec map-rec state]
  showoff.showoff.Rectable
  (to-rect [c] (posrec->rect position rec))

  showoff.showoff.Drawable
  (draw [c ctx] (draw-sprite ctx (:image rec) position))

  showoff.showoff.Tickable
  (tick [c] (cooldown-tick state))

  Iconic
  (icon [c] (:image rec))

  Collectable
  (collect [c]
    (when (is-cool? state)
      (remove-entity @*current-map* c)
      (swap! *bag* conj c)))

  Useable
  (use-thing [c user]
    (let [[x y w h] (to-rect user)
          tx (Math/ceil (+ x w))
          idx (coords->idx @*current-map* tx (Math/floor y))
          current (get-map-idx @*current-map* idx)]
      (when (= (:kind current) :skip)
        (set-map-idx @*current-map* idx map-rec)
        (remove-from-bag c)))))

(defrecord Fist [rec state]
  Iconic
  (icon [c] (:image rec))

  Useable
  (use-thing [c] nil))

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
       :breakable true
       :shape :rect}

      [102 102 102]
      {:kind :image
       :image (resize-nearest-neighbor sprites [16 32 16 16] dims)
       :collidable true
       :breakable true
       :shape :rect}

      [204 204 204]
      {:kind :image
       :image (resize-nearest-neighbor sprites [16 16 16 16] dims)
       :collidable true
       :breakable true
       :shape :rect}

      [0 0 255]
      {:kind :image
       :image (resize-nearest-neighbor sprites [32 32 16 16] dims)
       :collidable false}
      
      })

    (set! *player-sprite*
          {:left (resize-nearest-neighbor sprites [0 0 16 16] dims)
           :right (resize-nearest-neighbor sprites [16 0 16 16] dims)})

    (set! *collectables*
          {:key
           {:image (resize-nearest-neighbor sprites [32 0 16 24] [(nth dims 0)
                                                                  (* 1.5 (nth dims 1))])
            :dims [1 1.5]
            :spawn (fn [pos rec] (StaticCollectable. pos rec))
            }
           
           :jackhammer
           {:image (resize-nearest-neighbor sprites [48 0 16 16] dims)
            :dims [1 1]
            :spawn (fn [pos rec cooldown] (Jackhammer. pos rec cooldown (atom {})))}

           :rubble
           {:image (resize-nearest-neighbor sprites [48 32 16 16] dims)
            :dims [0.8 0.8]
            :spawn (fn [pos rec map-rec] (Rubble. pos rec map-rec (atom {:cooldown 0.5})))}
           })
    
    (let [fist-rec {:image (resize-nearest-neighbor sprites [48 16 16 16] dims)
                    :dims [1 1]}]
      (swap! *bag* conj (Fist. fist-rec (atom {}))))
    
    ))

(defn with-prepared-assets [callback]
  (with-loaded-font "graphics/basic-font.gif" *font-chars* [8 8] 2 (color [196 106 59])
    (fn [font]
      (set! *orange-font* font)))

  (with-img (str "graphics/hud.png?" (Math/random))
    (fn [hud]
      (let [[w h] (img-dims hud)]
        (set! *hud* (resize-nearest-neighbor hud [0 0 w h] [640 480])))
      
      (with-img (str "graphics/sprites.png?" (Math/random))
        (fn [sprites]
          (setup-symbols sprites)
          (with-img (str "graphics/world.gif?" (Math/random))
            (fn [map-img]
              (reset! *current-map* (load-map map-img *symbols*))
              (callback))))))))


(defn input-state []
  showoff.core.*command-state-map*)

(extend-type js/HTMLCanvasElement
  IHash
  (-hash [c] (goog.getUid c)))

(defn collectable? [o]
  (satisfies? Collectable o))

(defrecord Player [particle]
  showoff.showoff.Rectable
  (to-rect [player]
    (let [[x y] (:position @particle)]
      [(+ x 0.2) (+ y 0.1) 0.6 0.9]))

  showoff.showoff.Tickable
  (tick [player]
    (cooldown-tick particle)
    
    ;; record the last directon of motion so we can draw our sprite
    ;; facing that way
    (cond
     ((input-state) (.-LEFT gevents/KeyCodes))
     (swap! particle conj {:direction :left})

     ((input-state) (.-RIGHT gevents/KeyCodes))
     (swap! particle conj {:direction :right}))

    ;; see if we're trying to use something
    (when ((input-state) 32)
      (use-thing (first @*bag*) player))

    ;; cycle the bag
    (when (and (is-cool? particle) ((input-state) (.-DOWN gevents/KeyCodes)))
      (reset! *bag* (conj (into [] (rest @*bag*)) (first @*bag*)))
      (cooldown-start particle .3))
    
    ;; look around our neighboring rects for collectables
    (doseq [idx (rect->idxs @*current-map* (to-rect player))]
      (let [rec (get-map-idx @*current-map* idx)
            objects (:objects rec)
            collectables (filter collectable? @objects)]
        (doseq [collectable collectables]
          (when (rect-intersect (to-rect collectable) (to-rect player))
            (collect collectable)
            ;;(play-sound :powerup)
            ))))
    
    ;; motion and collision detectin
    (reset! particle (apply-particle-vs-map (integrate-particle @particle)
                                            @*current-map*
                                            (to-rect player)
                                            0.0))) ;; restitution
  )

(defn draw-player [ctx p]
  (let [particle (:particle p)
        sprite-key (or (:direction @particle) :right)
        sprite (sprite-key *player-sprite*)]
    (draw-sprite ctx sprite (:position @particle))))

;;; the particle provides keyboard interaction, jumping, etc
(def *player-speed* 40)
(def *player*
  (Player.
   (atom
    {:mass 5
     :position [5 3]
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

(defn add-collectable [key pos & more]
  (let [rec (*collectables* key)
        coll (apply (:spawn rec) pos rec more)]
    (add-entity @*current-map* coll)))

(def +viewport-spring-constant+ 50)
(def +viewport-drag-coefficient+ 3)
(def +viewport-max-displacement+ 2)

(def *viewport*
  (Viewport.
   [16 10]
   (atom
    {:mass 1
     :position [5 3]
     :velocity [0 0]
     
     ;; try to keep the player character basically centered
     :force-generators
     [(fn [p] (spring-force (vec-sub (:position @(:particle *player*))
                                     (vec-sub (rect-center (viewport-rect))
                                              [0 1]))
                            +viewport-max-displacement+
                            +viewport-spring-constant+))
      (drag-force-generator +viewport-drag-coefficient+)]})))

(defn setup-world []
  (add-collectable :jackhammer [3 4] 0.3)
  (dotimes [ii 10]
    (add-collectable :key [(+ 31 (* ii 2)) 2.5])))

(defn prepare-sound []
  (let [sounds {:resources ["sounds/music2.ogg"
                            "sounds/music2.mp3"]
                :autoplay "bg-music"
                :spritemap
                {:bg-music
                 {:start 0.0
                  :end 135
                  :loop true}
                 :powerup
                 {:start 136
                  :end 137}}}
        
        mgrconfig {"useGameLoop" true}]
    (set! jukebox.Manager (jukebox.Manager. (clj->js mgrconfig)))
    (set! *media-player* (jukebox.Player. (clj->js sounds)))))

(defn draw-world []
  (clear)
  (let [ctx (context)
        [vx vy _ _] (viewport-rect)]
    (.drawImage ctx *backdrop* (- (* vx 9)) (- (* vy 9)))
    (draw-map @*current-map*)
    (draw-entities)
    (draw-player ctx *player*)

    ;; draw the hud
    (let [[w h] (img-dims *hud*)]
      (.drawImage ctx *hud* 0 0 w h 0 0 640 480))

    ;; draw whatever is in the front of the bag
    (let [item (first @*bag*)]
      (.drawImage ctx (icon item) 593 430))
    ))

(defn game-loop []
  (cycle-once draw-world)
  ;(.loop jukebox.Manager)
  true)

(defn ^:export game []
  (let [screen-size [640 480]
        canvas (make-canvas screen-size)]
    
    (dom/appendChild (content) canvas)
    (set-display-and-viewport canvas [640 432] #(to-rect *viewport*))
    (prepare-input)
    (add-entity @*current-map* *viewport*)
    (add-entity @*current-map* *player*)
    (add-entity @*current-map* (Bag. *bag*))
    (with-prepared-assets
      (fn []
        (setup-world)
        ;(prepare-sound)
        (until-false game-loop)))))


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
    (with-prepared-assets
      (fn []
        (setup-world)
        (until-false game-loop)))))



