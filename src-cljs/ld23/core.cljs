(ns ld23.core
  (:use (showoff.showoff :only [get-img remove-entity add-entity supported-by-map
                                Rectable Tickable Drawable
                                vec-add vec-dot vec-scale vec-sub vec-unit
                                vec-mag vec-negate
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
                                set-map-idx coords->idx format get-pixel get-pixel-data])
        (showoff.core :only [content clj->js Viewport prepare-input
                             keyboard-velocity-generator until-false
                             jump-velocity-generator ground-friction-generator
                             ]))
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.events :as gevents)
            (goog.Timer :as timer)
            (goog.events.KeyHandler :as geventskey)
            (clojure.browser.event :as event)
            (clojure.browser.repl :as repl)))


(def *current-map* (atom nil))
(def *symbols* nil)

(def *hud-font* nil)
(def *font-chars* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\"?!./:")

(def *backdrop* (get-img (str "graphics/backdrop.png?" (Math/random))))
(def *player-sprite* nil)
(def *hud* nil)
(def *collectables* nil)
(def *bag* (atom []))
(def *media-player* nil)
(def *keys-collected* (atom 0))
(def *bricks-destroyed* (atom 0))

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
    (swap! *keys-collected* inc)
    (remove-entity @*current-map* c)))

(def dir-offset {:left [-1 0]
                 :right [1 0]
                 :above [0 -1]
                 :below [0 1]})

(defn kind-towards? [rect dir kind]
  (let [center (rect-center rect)
        test-point (vec-add center (dir-offset dir))
        idx (coords->idx @*current-map* test-point)
        rec (get-map-idx @*current-map* idx)]
    (when (kind rec)
      idx)))

(defn breakable-underneath? [rect]
  (kind-towards? rect :below :breakable))

;;; bag (we need to be able to tick this)
(defrecord Bag [contents]
  showoff.showoff.Tickable
  (tick [b] (doseq [item @contents]
              (tick item))))

(defn add-to-bag [item]
  (swap! *bag* conj item))

(defn remove-from-bag [item]
  (reset! *bag* (into [] (filter #(not (= item %)) @*bag*))))

(defn take-brick [idx]
  (let [rec (get-map-idx @*current-map* idx)]
    (set-map-idx @*current-map* idx {:kind :skip})
    (when (not (:used rec)) (swap! *bricks-destroyed* inc))
    rec))

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
        (add-entity @*current-map* (add-collectable :rubble (idx->coords @*current-map* idx) (take-brick idx)))
        (cooldown-start state max-cooldown)))))

(defrecord Blowtorch [position rec max-cooldown state]
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
      (if-let [idx (kind-towards? (to-rect user) (:direction @(:particle user)) :breakable)]
        (add-entity @*current-map* (add-collectable :rubble (idx->coords @*current-map* idx) (take-brick idx)))
        (cooldown-start state max-cooldown)))))

(defn fillable? [rec]
  (let [player-idx (coords->idx @*current-map* (rect-center (to-rect *player*)))
        rec-idx (coords->idx @*current-map* (:coords rec))]
    (and (= (:kind rec) :skip) (not (= player-idx rec-idx)))))

(defrecord ConsumableStack [kind rec contents]
  showoff.showoff.Tickable
  (tick [c] (doseq [item @contents]
              (tick item)))

  Iconic
  (icon [c] (:image rec))

  Useable
  (use-thing [c user]
    (let [item (first @contents)]
      (when (use-thing item user)
        (swap! contents rest)))
    
    (when (empty? @contents)
      (remove-from-bag c))))

(defn add-with-stacking [item kind rec]
  (if-let [stack (first (filter #(= (:kind %) kind) @*bag*))]
    (swap! (:contents stack) conj item)
    (swap! *bag* conj (ConsumableStack. kind rec (atom [item])))))

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
      (add-with-stacking c :rubble rec)))

  Useable
  (use-thing [c user]
    (let [below-fillable-idx (kind-towards? (to-rect user) :below fillable?)]
      (if (and below-fillable-idx (not (supported-by-map @*current-map* (to-rect user))))
        ;; put the brick below
        (set-map-idx @*current-map* below-fillable-idx (conj map-rec {:used true}))

        ;; otherwise, try the direction we're facing
        (when-let [idx (kind-towards? (to-rect user) (:direction @(:particle user)) fillable?)]
          (set-map-idx @*current-map* idx (conj map-rec {:used true}))
          true)))))

(defrecord Fist [rec state]
  Iconic
  (icon [c] (:image rec))

  Useable
  (use-thing [c] nil))

(defrecord Timer [start state on-finished]
  showoff.showoff.Tickable
  (tick [c]
    (let [last-time (or (:time @state) start)]
      (swap! state conj {:time (- last-time showoff.showoff.+secs-per-tick+)})
      (when (<= last-time 0)
        (on-finished)))))

(def *game-timer* nil)

(defn draw-timer [ctx timer]
  (let [time (Math/ceil (:time @(:state timer)))
        minutes (Math/floor (/ time 60))
        seconds (mod time 60)]
    (draw-text-centered ctx *hud-font* (format "%2d:%02d" minutes seconds) [330 455])))

(defn fill-template [pdata [px py] sym]
  (let [[dw dh] showoff.showoff.*tile-in-world-dims*
        [sw sh] (:dims pdata)
        pw (/ dw 16)
        ph (/ dh 16)
        num-pixels (* 16 16)
        canvas (make-canvas [dw dh])
        ctx (context canvas)
        [br bg bb] sym]
    (dotimes [ii num-pixels]
      (let [x (mod ii 16)
            y (Math/floor (/ ii 16))
            src-x (+ px x)
            src-y (+ py y)
            [scale _ _] (get-pixel pdata src-x src-y)
            scale (/ scale 255)]
        
        (set! (.-fillStyle ctx) (color [(Math/round (* scale br))
                                        (Math/round (* scale bg))
                                        (Math/round (* scale bb))]))
        (.fillRect ctx (Math/floor (* x pw)) (Math/floor (* y ph))
                   (Math/ceil pw) (Math/ceil ph))))

    {:kind :image
     :image canvas
     :collidable true
     :shape :rect
     :breakable true}))

(defn make-symbol-generator [base-symbols pdata templates [startx starty]]
  (let [cache (atom {})]
    (fn [sym]
      (if-let [result (base-symbols sym)]
        result
        (let [template (Math/floor (* templates (Math/random)))
              cache-key [sym template]]
          (if-let [result (@cache cache-key)]
            result
            (let [filled-template (fill-template pdata [(+ startx (* template 16)) starty] sym)]
              (swap! cache conj {cache-key filled-template})
              filled-template)))))))

(defn setup-symbols [sprites]
  (let [dims showoff.showoff.*tile-in-world-dims*
        pdata (get-pixel-data sprites)
        base-symbols {[255 255 255]
                      {:kind :skip}
      
                      [0 0 0]
                      {:kind :image
                       :image (resize-nearest-neighbor pdata [0 16 16 16] dims)
                       :collidable true
                       :shape :rect}

                      [0 255 0]
                      {:kind :image
                       :image (resize-nearest-neighbor pdata [0 32 16 16] dims)
                       :collidable true
                       :breakable true
                       :shape :rect}
                      
                      [102 102 102]
                      {:kind :image
                       :image (resize-nearest-neighbor pdata [16 32 16 16] dims)
                       :collidable true
                       :breakable true
                       :shape :rect}
                      
                      [204 204 204]
                      {:kind :image
                       :image (resize-nearest-neighbor pdata [16 16 16 16] dims)
                       :collidable true
                       :breakable true
                       :shape :rect}
                      
                      [0 0 255]
                      {:kind :image
                       :image (resize-nearest-neighbor pdata [32 32 16 16] dims)
                       :collidable false}
                      
                      }]
    
    (set! *symbols* (make-symbol-generator base-symbols pdata 3 [0 48]))

    (set! *player-sprite*
          {:left (resize-nearest-neighbor pdata [0 0 16 16] dims)
           :right (resize-nearest-neighbor pdata [16 0 16 16] dims)})

    (set! *collectables*
          {:key
           {:image (resize-nearest-neighbor pdata [32 0 16 16] dims)
            :dims [1 1]
            :spawn (fn [pos rec] (StaticCollectable. pos rec))
            }
           
           :jackhammer
           {:image (resize-nearest-neighbor pdata [48 0 16 16] dims)
            :dims [1 1]
            :spawn (fn [pos rec cooldown] (Jackhammer. pos rec cooldown (atom {})))}

           :blowtorch
           {:image (resize-nearest-neighbor pdata [48 48 16 16] dims)
            :dims [1 1]
            :spawn (fn [pos rec cooldown] (Blowtorch. pos rec cooldown (atom {})))}

           :rubble
           {:image (resize-nearest-neighbor pdata [48 32 16 16] dims)
            :dims [1 1]
            :spawn (fn [pos rec map-rec] (Rubble. pos rec map-rec (atom {:cooldown 0.2})))}
           })
    
    (let [fist-rec {:image (resize-nearest-neighbor pdata [48 16 16 16] dims)
                    :dims [1 1]}]
      (swap! *bag* conj (Fist. fist-rec (atom {}))))
    
    ))

(defn with-prepared-assets [callback]
  (with-loaded-font "graphics/basic-font.gif" *font-chars* [8 8] 2 (color [128 0 0])
    (fn [font]
      (set! *hud-font* font)))

  (with-img (str "graphics/hud.png?" (Math/random))
    (fn [hud]
      (set! *hud* (resize-nearest-neighbor (get-pixel-data hud) [640 480]))
      
      (with-img (str "graphics/sprites.png?" (Math/random))
        (fn [sprites]
          (setup-symbols sprites)
          (with-img (str "graphics/world2.gif?" (Math/random))
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
      [(+ x 0.1) (+ y 0.1) 0.8 0.9]))

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

    ;; reset cooldown if no command keys are down
    (when (not (or ((input-state) (.-DOWN gevents/KeyCodes))
                   ((input-state) 32)))
      (swap! particle conj {:cooldown 0}))

    ;; see if we're trying to use something
    (when (and (is-cool? particle) ((input-state) 32))
      (use-thing (first @*bag*) player)
      (cooldown-start particle .3))

    ;; cycle after our cooldown is done
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
    
    ;; motion and collision detection
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
     :position [5 5]
     :velocity [0 0]
    
     ;; bring to a stop quickly
     :force-generators
     [(drag-force-generator 1.0)
      (ground-friction-generator *current-map* #(to-rect *player*) 30)
      (gravity-force-generator 20)
      (keyboard-velocity-generator
       (.-LEFT gevents/KeyCodes) [(- *player-speed*) 0])
      (keyboard-velocity-generator
       (.-RIGHT gevents/KeyCodes) [*player-speed* 0])
      (jump-velocity-generator *current-map* #(to-rect *player*) 300 0.5)
      ]
     })))

(defn add-collectable [key pos & more]
  (let [rec (*collectables* key)
        coll (apply (:spawn rec) pos rec more)]
    (add-entity @*current-map* coll)))

(def +viewport-spring-constant+ 60)
(def +viewport-drag-coefficient+ 2)
(def +viewport-max-displacement+ 2)

(def *viewport*
  (Viewport.
   [20 15]
   (atom
    {:mass 1
     :position [5 5]
     :velocity [0 0]
     
     ;; try to keep the player character basically centered
     :force-generators
     [(fn [p] (spring-force (vec-sub (:position @(:particle *player*))
                                     (vec-sub (rect-center (viewport-rect))
                                              [0 2]))
                            +viewport-max-displacement+
                            +viewport-spring-constant+))
      (drag-force-generator +viewport-drag-coefficient+)

      ;; bring to rest if we're not moving very fast
      (fn [p]
        (let [spd (vec-mag (:velocity p))
              drag-dir (vec-negate (vec-unit (:velocity p)))]
          (vec-scale drag-dir (* spd 0.3))))]})))

(def *game-running* false)

(defn setup-world []
  (add-collectable :jackhammer [9 5] 0.3)
  (add-collectable :blowtorch [5 22] 0.3)
  (doseq [p [[21 24] [30 22] [31 22] [32 22] [33 22] [54 16] [59 39] [6 49] [7 49] [8 49] [9 49]]]
    (add-collectable :key p))

  (set! *game-timer*
        (Timer. 180 (atom {})
                (fn []
                  (set! *game-running* false)
                  (js/alert (format "You caused $%d worth of damage and found %d keys!"
                                    (* @*bricks-destroyed* 1000)
                                    @*keys-collected*)))))
  
  (add-entity @*current-map* *game-timer*)
  (set! *game-running* true))

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
      (.drawImage ctx (icon item) 594 435))

    (draw-timer ctx *game-timer*)
    ))

(defn game-loop []
  (cycle-once draw-world)
  (.loop jukebox.Manager)
  *game-running*)

(defn ^:export game []
  (let [screen-size [640 480]
        canvas (make-canvas screen-size)]
    
    (dom/appendChild (content) canvas)
    (set-display-and-viewport canvas [640 480] #(to-rect *viewport*))
    (prepare-input)
    (add-entity @*current-map* *viewport*)
    (add-entity @*current-map* *player*)
    (add-entity @*current-map* (Bag. *bag*))
    (with-prepared-assets
      (fn []
        (setup-world)
        (prepare-sound)
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



