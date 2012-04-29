(ns ld23.core
  (:use (showoff.showoff :only [get-img remove-entity add-entity clear-entities
                                supported-by-map
                                Rectable Tickable Drawable
                                vec-add vec-dot vec-scale vec-sub vec-unit
                                vec-mag vec-negate reset-tick-clock
                                rect-center viewport-rect clear display
                                tick-entities tick to-rect draw rect-intersect
                                rect-offset
                                drag-force-generator gravity-force-generator
                                integrate-particle spring-force
                                apply-particle-vs-map load-map draw-map
                                draw-sprite make-canvas get-img with-img
                                draw-entities img-dims context filled-rect
                                color map-collisions rect->idxs idx->coords
                                resize-nearest-neighbor record-vs-rect
                                set-display-and-viewport cycle-once
                                head-bumped-map with-loaded-font draw-text
                                draw-text-centered stats-string get-map-idx fill-style
                                set-map-idx coords->idx format get-pixel get-pixel-data])
        (showoff.core :only [content clj->js Viewport prepare-input
                             keyboard-velocity-generator until-false
                             jump-velocity-generator ground-friction-generator
                             request-animation by-id]))
  (:require (goog.dom :as dom)
            (goog.string :as string)
            (goog.events :as gevents)
            (goog.Timer :as timer)
            (goog.events.KeyHandler :as geventskey)
            (clojure.browser.event :as event)
            (clojure.browser.repl :as repl)))

(def *canvas* nil)
(def *current-map* (atom nil))
(def *symbols* nil)

(def *hud-font* nil)
(def *font-chars* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\"?!./:$")

(def *backdrop* (get-img (str "graphics/backdrop.png")))
(def *player-sprite* nil)
(def *money-icon* nil)
(def *hud* nil)
(def *collectables* nil)
(def *bag* (atom []))
(def *media-player* nil)
(def *keys-collected* (atom 0))
(def *total-keys* (atom 0))
(def *bricks-destroyed* (atom 0))
(def *debug-mode* false)

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
    (play-sound :powerup)
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

(defn empty-bag []
  (reset! *bag* []))

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

  Iconic
  (icon [c] (:image rec))
  
  Collectable
  (collect [c]
    (play-sound :pickup-item)
    (remove-entity @*current-map* c)
    (add-to-bag c))

  Useable
  (use-thing [c user]
    (if-let [idx (breakable-underneath? (to-rect user))]
      (do
        (play-sound :dig)
        (add-entity @*current-map* (add-collectable :rubble (idx->coords @*current-map* idx) (take-brick idx)))))))

(defrecord Blowtorch [position rec max-cooldown state]
  showoff.showoff.Rectable
  (to-rect [c] (posrec->rect position rec))

  showoff.showoff.Drawable
  (draw [c ctx] (draw-sprite ctx (:image rec) position))

  Iconic
  (icon [c] (:image rec))
  
  Collectable
  (collect [c]
    (play-sound :pickup-item)
    (remove-entity @*current-map* c)
    (add-to-bag c))

  Useable
  (use-thing [c user]
    (if-let [idx (kind-towards? (to-rect user) (:direction @(:particle user)) :breakable)]
      (do
        (play-sound :dig)
        (add-entity @*current-map* (add-collectable :rubble (idx->coords @*current-map* idx) (take-brick idx)))))))

(defn fillable? [rec]
  (let [player-idx (coords->idx @*current-map* (rect-center (to-rect *player*)))
        rec-idx (coords->idx @*current-map* (:coords rec))]
    (and (= (:kind rec) :skip) (not (= player-idx rec-idx)))))

(defrecord ConsumableStack [kind rec contents]
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
    (if-let [below-fillable-idx (kind-towards? (to-rect user) :below fillable?)]
      ;; if we can fill below, do that
      (set-map-idx @*current-map* below-fillable-idx (conj map-rec {:used true}))

      ;; otherwise, try the direction we're facing
      (when-let [idx (kind-towards? (to-rect user) (:direction @(:particle user)) fillable?)]
        (set-map-idx @*current-map* idx (conj map-rec {:used true}))))))

(defrecord Fist [rec state]
  Iconic
  (icon [c] (:image rec))

  Useable
  (use-thing [c] nil))

(defn timer-time [timer]
  (or (:time @(:state timer)) (:start timer)))

(defrecord Timer [start state]
  showoff.showoff.Tickable
  (tick [c]
    (let [last-time (timer-time c)]
      (swap! state conj {:time (- last-time showoff.showoff.+secs-per-tick+)})
      (when (<= last-time 0)
        (swap! state conj {:time 0})))))

(def *game-timer* nil)

(defn draw-timer [ctx timer]
  (let [time (Math/ceil (:time @(:state timer)))
        minutes (Math/floor (/ time 60))
        seconds (mod time 60)]
    (draw-text-centered ctx *hud-font* (format "%2d:%02d" minutes seconds) [55 452])))

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
           :right (resize-nearest-neighbor pdata [16 0 16 16] dims)
           :right-walk1 (resize-nearest-neighbor pdata [0 64 16 16] dims)
           :right-walk2 (resize-nearest-neighbor pdata [16 64 16 16] dims)
           :left-walk1 (resize-nearest-neighbor pdata [32 64 16 16] dims)
           :left-walk2 (resize-nearest-neighbor pdata [48 64 16 16] dims)}
          )

    (set! *money-icon* (resize-nearest-neighbor pdata [64 0 16 16] dims))
    
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

           ;; not really collectable... that would be weird.
           :fist
           {:image (resize-nearest-neighbor pdata [48 16 16 16] dims)
            :dims [1 1]}
           })
    ))

(defn with-prepared-assets [callback]
  (with-loaded-font "graphics/basic-font.gif" *font-chars* [8 8] 2 (color [128 0 0])
    (fn [font]
      (set! *hud-font* font)))

  (with-img (str "graphics/hud.png")
    (fn [hud]
      (set! *hud* (resize-nearest-neighbor (get-pixel-data hud) [640 480]))
      
      (with-img (str "graphics/sprites.png")
        (fn [sprites]
          (setup-symbols sprites)
          (callback))))))

(defn with-loaded-map [callback]
  (with-img (str "graphics/world2.gif")
    (fn [map-img]
      (reset! *current-map* (load-map map-img *symbols*))
      (callback))))

(def ^:dynamic *command-state-override* nil)

(defn input-state []
  (if *command-state-override*
    *command-state-override*
    showoff.core.*command-state-map*))

(extend-type js/HTMLCanvasElement
  IHash
  (-hash [c] (goog.getUid c)))

(defn collectable? [o]
  (satisfies? Collectable o))

(defn particle-position [a]
  (@a :position))

(defn particle-velocity [a]
  (@a :velocity))

(def +max-speed+ 10)
(def +player-accel+ 0.5)
(def +friction+ (/ +max-speed+ (+ +max-speed+ +player-accel+)))
(def +min-speed+ (* +player-accel+ +friction+))
(def +jump-speed+ 12)
(def +gravity+ 1)
(def +max-fall+ 12)

(defn move-check-map-collision [map [dx dy] rect on-x-collide on-y-collide]
  (let [x-moved-rect (rect-offset rect [dx 0])
        x-offset (if-let [hit-idx (first (map-collisions map x-moved-rect))]
                   (do (on-x-collide hit-idx) 0)
                   dx)

        y-moved-rect (rect-offset rect [x-offset dy])
        y-offset (if-let [hit-idx (first (map-collisions map y-moved-rect))]
                   (do (on-y-collide hit-idx) 0)
                   dy)]
    
    [x-offset y-offset]))

(defn player-movement [player particle]
  ;; motion and collision detection
  (when ((input-state) (.-RIGHT gevents/KeyCodes))
    (swap! particle conj {:velocity (vec-add (particle-velocity particle)
                                             [+player-accel+ 0])}))
  
  (when ((input-state) (.-LEFT gevents/KeyCodes))
    (swap! particle conj {:velocity (vec-add (particle-velocity particle)
                                             [(- +player-accel+) 0])}))
  (if (supported-by-map @*current-map* (to-rect player))
    ;; on the ground, check for jump
    (when ((input-state) (.-UP gevents/KeyCodes))
      (play-sound :jump)
      (swap! particle conj {:velocity (vec-add (particle-velocity particle)
                                               [0 (- +jump-speed+)])}))
    
    ;; in the air, apply gravity
    (let [[xvel yvel] (particle-velocity particle)
          yvel (min +max-fall+ (+ yvel +gravity+))]
      (swap! particle conj {:velocity [xvel yvel]})))
  
  ;; apply friction (only horizontally) and stop sliding
  (let [[xvel yvel] (particle-velocity particle)
        xvel (* +friction+ xvel)
        xvel (if (< (Math/abs xvel) +min-speed+)
               0
               xvel)]
    (swap! particle conj {:velocity [xvel yvel]}))
  

  ;; velocity integration and collision detection
  (swap!
   particle conj
   {
    :position
    (vec-add
     (particle-position particle)
     (move-check-map-collision
      @*current-map*
      (vec-scale (particle-velocity particle)
                 showoff.showoff.+secs-per-tick+)
      (to-rect player)
      ;; on-x-collide
      (fn [idx]
        (let [[vx vy] (particle-velocity particle)]
          (swap! particle conj {:velocity [0 vy]})
          true))
      
      ;; on-y-collide
      (fn [idx]
        (let [[vx vy] (particle-velocity particle)]
          (swap! particle conj {:velocity [vx 0]})
          true))))}))

(defn timer-value [mutable place]
  (or (place @mutable) 0))

(defn timer-tick [mutable place]
  (let [last-value (timer-value mutable place)]
    (swap! mutable conj {place (inc last-value)})))

(defn timer-scaled [mutable place scale]
  (Math/floor (/ (timer-value mutable place) scale)))

(defrecord Player [particle]
  showoff.showoff.Rectable
  (to-rect [player]
    (let [[x y] (:position @particle)]
      [(+ x 0.1) (+ y 0.2) 0.8 0.8]))

  showoff.showoff.Tickable
  (tick [player]
    (cooldown-tick particle)
    (timer-tick particle :walk-timer)
    
    ;; record the last directon of motion so we can draw our sprite
    ;; facing that way
    (cond
     ((input-state) (.-LEFT gevents/KeyCodes))
     (swap! particle conj {:direction :left})

     ((input-state) (.-RIGHT gevents/KeyCodes))
     (swap! particle conj {:direction :right}))

    ;; mark ourselves walking or not-walking for animation
    (if (or ((input-state) (.-LEFT gevents/KeyCodes))
            ((input-state) (.-RIGHT gevents/KeyCodes)))
      (swap! particle conj {:walking true})
      (swap! particle conj {:walking false}))

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
            (collect collectable)))))

    (player-movement player particle)))

(defn walk-frame [direction frame]
  (let [frameset (if (= direction :left)
                   [:left-walk1 :left :left-walk2 :left]
                   [:right-walk1 :right :right-walk2 :right])]
    (frameset (mod frame 4))))

(defn draw-player-entity [ctx p]
  (let [particle @(:particle p)
        direction (or (:direction particle) :right)
        sprite-key (if (:walking particle)
                     (walk-frame direction (timer-scaled (:particle p) :walk-timer 4))
                     direction)
        sprite (*player-sprite* sprite-key)]
    (draw-sprite ctx sprite (:position particle))))

;;; the particle provides keyboard interaction, jumping, etc
(def *player* nil)

(defn add-collectable [key pos & more]
  (let [rec (*collectables* key)
        coll (apply (:spawn rec) pos rec more)]
    (add-entity @*current-map* coll)))

(def +viewport-spring-constant+ 60)
(def +viewport-drag-coefficient+ 2)
(def +viewport-max-displacement+ 2)

(def *viewport* nil)

(defn add-keys [locations]
  (doseq [loc locations]
    (add-collectable :key loc))
  (reset! *total-keys* (count locations)))

(def *player-speed* 6)

(defn setup-world [callback]
  (empty-bag)
  (clear-entities)
  (reset! *keys-collected* 0)
  (reset! *bricks-destroyed* 0)
  
  (swap! *bag* conj (Fist. (:fist *collectables*) (atom {})))

  (set!
   *player*
   (Player.
    (atom
     {:mass 5
      :position [5 5]
      :velocity [0 0]
    
      })))

  (set!
   *viewport*
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

  (set-display-and-viewport *canvas* [640 480] #(to-rect *viewport*))
  
  (add-collectable :jackhammer [9 5] 0.3)
  (add-collectable :blowtorch [5 22] 0.3)
  (add-keys [[21 24] [30 22] [31 22] [32 22] [33 22] [54 16] [59 39] [6 49] [7 49] [8 49] [9 49] [22 29]])

  (add-entity @*current-map* *viewport*)
  (add-entity @*current-map* *player*)
  (add-entity @*current-map* (Bag. *bag*))
  
  (set! *game-timer* (Timer. (* 4 60) (atom {})))
  (add-entity @*current-map* *game-timer*)
  (reset-tick-clock)
  
  (callback))

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
                  :end 137}
                 :dig
                 {:start 140
                  :end 140.2}
                 :jump
                 {:start 144
                  :end 144.2}
                 :pickup-coin
                 {:start 148
                  :end 148.1}
                 :pickup-item
                 {:start 152
                  :end 152.4}
                 }}
        
        mgrconfig {"useGameLoop" true}]
    (set! jukebox.Manager (jukebox.Manager. (clj->js mgrconfig)))
    (set! *media-player* (jukebox.Player. (clj->js sounds)))))

(defn draw-world [ticks]
  ;; only draw if we actually ticked
  (when (> ticks 0)
    (clear)
    (let [ctx (context)
          [vx vy _ _] (viewport-rect)]
      (.drawImage ctx *backdrop* (- 0 50 (* 13 vx)) (- 0 10 (* 13 vy)))
      (draw-map @*current-map*)
      (draw-entities)

      ;; draw the hitrect
      ;;(filled-rect ctx (to-rect *player*) (color [255 0 255]))
      
      (draw-player-entity ctx *player*)
      
      ;; draw the hud
      (let [[w h] (img-dims *hud*)
            key-img (:image (:key *collectables*))]
        (.drawImage ctx *hud* 0 0 w h 0 0 640 480)
        (.drawImage ctx key-img (* 2 73) (* 2 218))
        (draw-text ctx *hud-font* (str @*keys-collected*) [180 444])

        (.drawImage ctx *money-icon* (* 2 114) (* 2 218))
        (draw-text ctx *hud-font* (str "$" (* 1000 @*bricks-destroyed*)) [268 444]))
      
      ;; draw whatever is in the front of the bag
      (let [item (first @*bag*)]
        (.drawImage ctx (icon item) 592 436))
      
      (draw-timer ctx *game-timer*)))

  (when *debug-mode*
   (set! (.-innerHTML (by-id "console")) (pr-str (input-state))))
  
  ;; next state
  (if (= (timer-time *game-timer*) 0)
    :show-score
    :game))

(def *base-dialog* nil)
(def *instructions* nil)

(defn with-dialog-assets [callback]
  (with-img "graphics/dialog.png"
    (fn [dialog]
      (set! *base-dialog* (resize-nearest-neighbor (get-pixel-data dialog) [640 480]))
      (with-img "graphics/instructions.png"
        (fn [instr]
          (set! *instructions* (resize-nearest-neighbor (get-pixel-data instr) [648 480]))
          (callback))))))

(def *instruction-time* (atom nil))

(defn once-only-setup [callback]
  (with-prepared-assets
    (fn []
      (with-dialog-assets
        (fn []
          (callback))))))

(def +total-instruction-time+ 30)

(defn prepare-instructions [callback]
  (reset! *instruction-time* +total-instruction-time+)
  (set-display-and-viewport *canvas* [640 480] #(vector 0 0 20 15))
  (reset-tick-clock)
  (callback))

(defn any-keys-pressed? []
  (not (empty? (input-state))))

(defn instructions-screen [ticks]
  (let [ctx (context)]
    (let [factor (/ @*instruction-time* +total-instruction-time+)
          v (* 2 Math/PI 3 factor)
          xoff (Math/sin v)
          hopv (- (/ (mod factor 0.03) 0.03) 0.5)
          yoff (- (* hopv hopv 2))
          dir (if (< (Math/cos v) 0)
                :right
                :left)]
      ;; just the backdrop
      (.drawImage ctx *backdrop* (- (* factor 600)) 0)
      
      ;; dialog
      (.drawImage ctx *base-dialog* 0 0)
      
      ;; instructions icons
      (.drawImage ctx *instructions* 0 0)

      ;; left/right character
      ;(draw-player ctx [(+ 9 xoff) 1.5] dir)

      ;; up/down character
      ;(draw-player ctx [15 (- 1.1 yoff)] :right)

      
      (let [item-number (Math/floor (* (/ (mod factor 0.1) 0.1) 3))
            item-key ([:jackhammer :blowtorch :rubble] item-number)
            item-img (:image (*collectables* item-key))]

        ;; the icon in the bar
        (.drawImage ctx item-img (* 2 99) (* 2 79))

        ;; the player holding the icon
        (.drawImage ctx item-img (* 2 238) (* 2 80))
        ;(draw-player ctx [16 5] :left)

        ))
    
    (reset! *instruction-time*
            (- @*instruction-time*
               (* ticks showoff.showoff.+secs-per-tick+)))
    
    (if (or (<= @*instruction-time* 0)
            (any-keys-pressed?))
      :setup-map
      :instructions)))

(def *scorescreen-cooldown* (atom {}))

(defn prepare-scorescreen [callback]
  (cooldown-start *scorescreen-cooldown* 1)
  (callback))

(defn scorescreen [ticks]
  ;; don't clear, leave whatever used to be on the screen
  (let [ctx (context)
        key-img (:image (:key *collectables*))]
    (.drawImage ctx *base-dialog* 0 0)
    (draw-text-centered ctx *hud-font* "Time is Up!" [320 100])
    (.drawImage ctx key-img 216 150)
    (draw-text ctx *hud-font* (format "%d of %d" @*keys-collected* @*total-keys*) [264 158])
    (.drawImage ctx *money-icon* 216 182)
    (draw-text ctx *hud-font* (format "$%d" (* 1000 @*bricks-destroyed*)) [264 190])
    (draw-text ctx *hud-font* "of damage done" [264 222])

    (draw-text ctx *hud-font* "Press a Key" [264 286]))
  
  (cooldown-tick *scorescreen-cooldown*)
  
  (if (and (is-cool? *scorescreen-cooldown*) (any-keys-pressed?))
    :setup-map
    :show-score))

(def *game-states*
  {:start
   {:setup once-only-setup
    :after-ticks (fn [] :setup-map)}

   :instructions
   {:setup prepare-instructions
    :after-ticks instructions-screen}

   :setup-map
   {:setup with-loaded-map
    :after-ticks (fn [] :game)}
   
   :game
   {:setup setup-world
    :after-ticks draw-world}

   :show-score
   {:setup prepare-scorescreen
    :after-ticks scorescreen}
   
   })

(def *current-game-state* (atom nil))

(defn perform-after-ticks [ticks]
  (let [after-ticks (:after-ticks (@*current-game-state* *game-states*))]
    (after-ticks ticks)))

(defn with-changed-game-state [new-state callback]
  (let [change-complete (fn []
                          (reset! *current-game-state* new-state)
                          (callback))]
    (if (not (= @*current-game-state* new-state))
      ;; actually changing state
      (if-let [setup (:setup (new-state *game-states*))]
        ;; we have a setup function, run and finish the state change
        ;; when it's done
        (setup change-complete)
        ;; no setup function
        (change-complete))
      
      ;; not really changing state so we don't look for a setup function
      (change-complete))))


(defn game-loop []
  ;; give the sound system a chance to run
  (.loop jukebox.Manager)
  
  (if (= @*current-game-state* nil)
    ;; set the state to start
    (with-changed-game-state :start
      (fn []
        (perform-after-ticks 0)
        (request-animation game-loop (display))))

    ;; otherwise, perform in our current state
    (let [next-state (cycle-once perform-after-ticks)]
      (with-changed-game-state next-state
        (fn []
          (request-animation game-loop (display)))))))

(defn ^:export game []
  (let [screen-size [640 480]
        canvas (make-canvas screen-size)]
    
    (dom/appendChild (content) canvas)
    (set! *canvas* canvas)
    (set-display-and-viewport *canvas* [640 480] #(vector 0 0 20 15))
    
    (prepare-input)
    (prepare-sound)
    
    (game-loop)))


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



