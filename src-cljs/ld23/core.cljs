(ns ld23.core
  (:use (showoff.showoff :only [remove-entity add-entity clear-entities
                                supported-by-map
                                Rectable Tickable Drawable MapIndexed
                                reset-tick-clock
                                viewport-rect display
                                tick-entities tick to-rect draw
                                drag-force-generator
                                integrate-particle spring-force
                                apply-particle-vs-map
                                draw-sprite inverse-transform-position
                                draw-entities filled-rect
                                color move-check-map-collision
                                record-vs-rect inverse-transform-dims
                                set-display-and-viewport
                                head-bumped-map with-loaded-font draw-text
                                draw-text-centered]))
  
  (:require [showoff.vec :as vec]
            [showoff.rect :as rect]
            [showoff.gfx :as gfx]
            [showoff.map :as map]
            [showoff.input :as input]
            [showoff.ai :as brain]
            [showoff.states :as states]
            [showoff.utils :as utils]
            [showoff.events :as event]
            [showoff.timer :as timer]
            [ld23.recorded :as recorded]
            [goog.events :as gevents]
            [goog.events.KeyHandler :as geventskey]
            [goog.Uri :as uri]
            [goog.dom :as dom]
            [clojure.browser.repl :as repl]))

(def *canvas* nil)
(def *current-map* (atom nil))
(def *symbols* nil)
(def *overlay-entities* (atom nil))

(def *hud-font* nil)
(def *font-chars* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\"?!./:$")
(def *base-dialog* nil)

(def *player* nil)

(def *backdrop* nil)
(def *player-sprite* nil)
(def *light-sprite* nil)
(def *money-icon* nil)
(def *hud* nil)
(def *collectables* nil)
(def *bag* (atom []))
(def *media-player* nil)
(def *keys-collected* (atom 0))
(def *kind-totals* (atom {}))
(def *bricks-destroyed* (atom 0))
(def *debug-mode* false)

(def *overlay-assets*
  (atom
   {:light
    {:image (utils/curry gfx/with-img "graphics/light.png")}
    
    :light-helmet
    {:image (utils/curry gfx/with-img "graphics/light_helmet.png")}
    
    :sun
    {:image (utils/curry gfx/with-img "graphics/sun.png")}}))

(defn kind-total [kind]
  (or (kind @*kind-totals*) 0))

(defn play-sound [key]
  (when *media-player*
    (.play *media-player* (name key) false)))

(defprotocol Collectable
  (collect [c]))

(defprotocol Useable
  (use-thing [obj user]))

(defprotocol Iconic
  (icon [obj]))

(defprotocol Enablable
  (enable [obj]))

(extend-type default
  Enablable
  (enable [_] nil))

(defn spawn-item
  ([key]
     (spawn-item key [0 0]))
  
  ([key pos & more]
     (let [rec (*collectables* key)]
       (conj
        (apply (:spawn rec) pos rec more)
        {:spawn-key key :spawn-position pos}))))

(defn add-collectable [key pos & more]
  (let [coll (apply spawn-item key pos more)]
    (add-entity @*current-map* coll)
    (swap! *kind-totals* conj {key (inc (kind-total key))})
    coll))

(defn posrec->rect [position rec]
  (let [[w h] (:dims rec)
        [px py] (vec/add position [(* 0.5 (- 1 w))
                                   (* 0.5 (- 1 h))])]
    [px py w h]))

(defn cooldown-tick
  ([obj] (cooldown-tick obj :cooldown))

  ([obj slot]
     (let [cooldown (or (slot @obj) 0)]
       (swap! obj conj {slot (- cooldown showoff.showoff.+secs-per-tick+)}))))

(defn is-cool?
  ([obj] (is-cool? obj :cooldown))

  ([obj slot]
     (<= (or (slot @obj) 0) 0)))

(defn cooldown-start
  ([obj max-cooldown] (cooldown-start obj max-cooldown :cooldown))

  ([obj max-cooldown slot]
     (swap! obj conj {slot max-cooldown})))

(defrecord Viewport [dims particle]
  Rectable
  (to-rect [vp]
    (let [[w h] dims
          [x y] (:position @particle)]
      [x y w h])) ;; expressed in tiles

  Tickable
  (tick [vp]
    (reset! particle (integrate-particle @particle)))

  MapIndexed
  (map-indexed? [vp] false))

(defrecord OverlayImage [position img]
  Rectable
  (to-rect [obj]
    (let [[w h] (inverse-transform-dims (gfx/img-dims img))
          [x y] position]
      [x y w h]))

  Drawable
  (draw [obj ctx]
    (draw-sprite ctx img position)))

(defrecord StaticCollectable [position rec]
  Rectable
  (to-rect [c] (posrec->rect position rec))

  Drawable
  (draw [c ctx] (draw-sprite ctx (:image rec) position))

  Collectable
  (collect [c]
    (swap! *keys-collected* inc)
    (play-sound :pickup-item)
    (remove-entity @*current-map* c)))

(defn breakable-underneath? [rect]
  (map/kind-towards? @*current-map* rect :below :breakable))

(defn collidable-underneath? [rect]
  (map/kind-towards? @*current-map* rect :below :collidable))

;;; bag (we need to be able to tick this)
(defrecord Bag [contents]
  Tickable
  (tick [b] (doseq [item @contents]
              (tick item))))

(defn item-key [e]
  "spawn-item makes sure that this is a valid key on any spawned
thing"
  (:spawn-key e))

(defn item-position [e]
  "the position that was given to spawn-item"
  (:spawn-position e))

(defn item-spec [e]
  (*collectables* (item-key e)))

(defn bag-behavior [e]
  (:bag-behavior (item-spec e)))

(defmulti add-to-bag bag-behavior)

(defmethod add-to-bag nil [item]
  (swap! *bag* conj item))

(defn remove-from-bag [item]
  (reset! *bag* (into [] (filter #(not (= item %)) @*bag*))))

(defn empty-bag []
  (reset! *bag* []))

(defn take-brick [idx]
  (let [rec (map/get-map-idx @*current-map* idx)]
    (map/set-map-idx @*current-map* idx {:kind :skip})
    (when (not (:used rec)) (swap! *bricks-destroyed* inc))
    rec))

(defrecord Switch [position state target]
  Rectable
  (to-rect [c]
    (let [[x y] position]
      [(+ x (/ 5 16)) (+ y (/ 3 16)) (/ 6 16) (/ 10 16)]))
  
  Drawable
  (draw [c ctx]
    (let [onoff (if @state :on :off)
          sprite (-> *light-sprite* onoff :image)]
      (draw-sprite ctx sprite position)))

  Enablable
  (enable [c]
    (swap! state not)
    (enable target)))

(defrecord SwitchableLight [position state]
  Rectable
  (to-rect [obj]
    (let [[w h] (inverse-transform-dims (gfx/img-dims (@*overlay-assets* :light-helmet)))
          [x y] position]
      [x y w h]))

  Drawable
  (draw [c ctx]
    (when @state
      (draw-sprite ctx (-> @*overlay-assets* :light :image) position))
    (draw-sprite ctx (-> @*overlay-assets* :light-helmet :image) position))

  Enablable
  (enable [c]
    (swap! state not)))

(defrecord Jackhammer [position rec state]
  Rectable
  (to-rect [c] (posrec->rect position rec))

  Drawable
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
        (add-entity @*current-map* (add-collectable :rubble (map/idx->coords @*current-map* idx) (take-brick idx)))))))

(defrecord Blowtorch [position rec state]
  Rectable
  (to-rect [c] (posrec->rect position rec))

  Drawable
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
    (if-let [idx (map/kind-towards? @*current-map* (to-rect user)
                                    (:direction @(:particle user))
                                    :breakable)]
      (do
        (play-sound :dig)
        (add-entity @*current-map* (add-collectable :rubble (map/idx->coords @*current-map* idx) (take-brick idx)))))))

(defn fillable? [rec]
  (let [[tx ty] (:coords rec)
        rec-rect [tx ty 1 1]]
    (and (= (:kind rec) :skip) (not (rect/intersect (to-rect *player*) rec-rect)))))

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

(defmethod add-to-bag :stack [item]
  (let [key (item-key item)
        spec (item-spec item)]
    (if-let [stack (first (filter #(= (:kind %) key) @*bag*))]
      (swap! (:contents stack) conj item)
      (swap! *bag* conj (ConsumableStack. key spec (atom [item]))))))

(defrecord Rubble [position rec map-rec]
  Rectable
  (to-rect [c] (posrec->rect position rec))

  Drawable
  (draw [c ctx] (draw-sprite ctx (:image rec) position))

  Iconic
  (icon [c] (:image rec))

  Collectable
  (collect [c]
    (remove-entity @*current-map* c)
    (add-to-bag c))

  Useable
  (use-thing [c user]
    (if-let [below-fillable-idx (map/kind-towards? @*current-map* (to-rect user)
                                                   :below fillable?)]
      ;; if we can fill below, do that
      (map/set-map-idx @*current-map* below-fillable-idx (conj map-rec {:used true}))

      ;; otherwise, try the direction we're facing
      (when-let [idx (map/kind-towards? @*current-map* (to-rect user)
                                        (:direction @(:particle user)) fillable?)]
        (map/set-map-idx @*current-map* idx (conj map-rec {:used true})))))

  IHash
  (-hash [c]
    (.getUid js/goog c)))

(defrecord Fist [rec]
  Iconic
  (icon [c] (:image rec))

  Useable
  (use-thing [c player]
    ;; look around for something to enable
    (map/with-objects-in-rect @*current-map* (to-rect player)
      (fn [obj]
        (enable obj)))))

(defn timer-time [timer]
  (or (:time @(:state timer)) 0))

(defrecord Timer [state]
  Tickable
  (tick [c]
    (let [last-time (timer-time c)]
      (swap! state conj {:time (+ last-time showoff.showoff.+secs-per-tick+)}))))

(def *game-timer* nil)

(defn draw-timer
  ([ctx timer]
     (draw-timer ctx timer [55 452]))

  ([ctx timer position]
     (let [time (Math/floor (:time @(:state timer)))
           minutes (Math/floor (/ time 60))
           seconds (mod time 60)]
       (draw-text-centered
        ctx *hud-font*
        (utils/format "%2d:%02d" minutes seconds) position))))

(defn fill-template [pdata [px py] sym]
  (let [src-rect [px py 16 16]
        [r g b] sym
        canvas (gfx/map-nearest-neighbor
                pdata src-rect showoff.showoff.*tile-in-world-dims*
                (fn [dest-data dest-base src-data src-base]
                  (let [scale (/ (aget src-data src-base) 255)]
                    (aset dest-data dest-base (* scale r))
                    (aset dest-data (+ 1 dest-base) (* scale g))
                    (aset dest-data (+ 2 dest-base) (* scale b))
                    (aset dest-data (+ 3 dest-base) 255))))]

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
        pdata (gfx/get-pixel-data sprites)
        base-symbols (gfx/slice-sprites
                      pdata
                      {[255 255 255]
                       {:kind :skip}
                       
                       [0 0 0]
                       {:kind :image
                        :image [0 1]
                        :collidable true
                        :shape :rect}

                       [0 255 0]
                       {:kind :image
                        :image [0 2]
                        :collidable true
                        :breakable true
                        :shape :rect}
                      
                       [102 102 102]
                       {:kind :image
                        :image [1 2]
                        :collidable true
                        :breakable true
                        :shape :rect}
                      
                       [204 204 204]
                       {:kind :image
                        :image [1 1]
                        :collidable true
                        :breakable true
                        :shape :rect}
                      
                       [0 0 255]
                       {:kind :image
                        :image [2 2]
                        :collidable false}
                      
                       })]
    
    (set! *symbols* (make-symbol-generator base-symbols pdata 3 [0 48]))

    (set! *player-sprite*
          (gfx/slice-sprites
           pdata
           {:left {:image [0 0]}
            :right {:image [1 0]}
            :right-walk1 {:image [0 4]}
            :right-walk2 {:image [1 4]}
            :left-walk1 {:image [2 4]}
            :left-walk2 {:image [3 4]}
            :fall-right {:image [4 1]}
            :fall-left {:image [4 2]}}))

    (set! *light-sprite*
          (gfx/slice-sprites
           pdata
           {:off {:image [4 3]}
            :on {:image [4 4]}}))
    
    (set! *money-icon* (gfx/resize-nearest-neighbor pdata [64 0 16 16] dims))
    
    (set! *collectables*
          (gfx/slice-sprites
           pdata
           {:key
            {:image [2 0]
             :dims [1 1]
             :spawn (fn [pos rec] (StaticCollectable. pos rec))
             }
           
            :jackhammer
            {:image [3 0]
             :dims [1 1]
             :spawn (fn [pos rec] (Jackhammer. pos rec (atom {})))}

            :blowtorch
            {:image [3 3]
             :dims [1 1]
             :spawn (fn [pos rec] (Blowtorch. pos rec (atom {})))}

            :rubble
            {:image [3 2]
             :dims [0.7 0.8]
             :spawn (fn
                      ([pos rec] (Rubble. pos rec (*symbols* [255 0 255])))
                      ([pos rec map-rec] (Rubble. pos rec map-rec)))
             :bag-behavior :stack}

            :fist
            {:image [3 1]
             :dims [1 1]
             :spawn (fn [pos rec] (Fist. rec))}}))))

(def *current-level-index* (atom 0))

(def *levels*
  [
   {:backdrop "graphics/backdrop.png"
    :initial-bag [:fist]
    :player [5 5]
    :overlays []
    :items
    {:blowtorch [[5 22]]
     :jackhammer [[9 5]]
     :key [[21 24] [30 22] [31 22] [32 22] [33 22] [59 39] [6 49] [7 49] [8 49] [9 49] [22 29] [58 59] [8 56] [47 31]]}
    :map "graphics/world2.gif"
    :level-setup-function :house
    }

   {:initial-bag [:fist], :player [2 60], :overlays [[:sun [14 8]]], :items {:blowtorch [[61 60]], :jackhammer [[30 27]], :key [[3 60] [10 53] [9 53] [8 53] [57 53] [50 36] [50 35] [12 47] [33 28] [31 28] [9 52] [8 52] [8 51] [45 41] [19 39] [41 26]]}, :map "graphics/world4.gif"}


   {:backdrop "graphics/backdrop.png"
    :initial-bag [:fist]
    :player [28 17]
    :items
    {:blowtorch []
     :rubble [[43 31] [43 32] [43 33]]
     :jackhammer []
     :key [[43 17] [43 30] [17 28] [12 19] [10 21]]}
    :map "graphics/world3.gif"}
   ])

(defn current-level []
  (let [idx @*current-level-index*]
    (*levels* idx)))

(defmulti level-specific-setup :level-setup-function)

(defmethod level-specific-setup nil
  [level]
  nil)

(defmethod level-specific-setup :house
  [level]
  (let [light (SwitchableLight. [38 17] (atom true))]
    (swap! *overlay-entities* conj light)
    (add-entity @*current-map* (Switch. [51 29] (atom false) light))))

(defn cycle-next-level []
  (swap! *current-level-index* (fn [v] (mod (inc v) (count *levels*)))))

(defn with-level-assets [callback]
  (utils/with-loaded-assets
    {:map
     (utils/curry gfx/with-img ((current-level) :map))

     :backdrop
     (utils/curry gfx/with-img ((current-level) :backdrop))}

    (fn [assets]
      (reset! *current-map* (map/load (:map assets) *symbols*))
      (set! *backdrop* (:backdrop assets))

      (callback))))

(defn with-loaded-images [spec callback]
  "execute the asset loaders found under the :image key and then
replace the :image key with the result"
  (let [map-keys (keys spec)
        image-loaders (map #(-> spec % :image) map-keys)
        asset-map (zipmap map-keys image-loaders)]
    (utils/with-loaded-assets asset-map
      (fn [assets]
        (let [with-images (map #(conj (% spec) {:image (% assets)}) map-keys)]
          (callback (zipmap map-keys with-images)))))))

(defn with-persistent-assets [callback]
  (utils/with-loaded-assets
    {:font
     (utils/curry with-loaded-font "graphics/basic-font.gif" *font-chars* [8 8] 2 [128 0 0])

     :hud
     (utils/curry gfx/with-img "graphics/hud.png")

     :sprites
     (utils/curry gfx/with-img "graphics/sprites.png")

     :dialog
     (utils/curry gfx/with-img "graphics/dialog.png")

     :overlays
     (utils/curry with-loaded-images @*overlay-assets*)
     
     }

    (fn [assets]
      (set! *hud-font* (:font assets))
      (set! *hud* (gfx/resize-nearest-neighbor (gfx/get-pixel-data (:hud assets)) [640 480]))
      (set! *base-dialog* (gfx/resize-nearest-neighbor (gfx/get-pixel-data (:dialog assets)) [640 480]))
      (setup-symbols (:sprites assets))
      (reset! *overlay-assets* (:overlays assets))

      (callback))))

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
(def +player-accel+ 2)
(def +friction+ (/ +max-speed+ (+ +max-speed+ +player-accel+)))
(def +min-speed+ (* +player-accel+ +friction+))
(def +jump-speed+ 12)
(def +gravity+ 1)
(def +max-fall+ 12)

(defn player-movement [player particle brain]
  ;; motion and collision detection
  (cooldown-tick particle :jump-timer)
  (when (brain/state? brain :right)
    (swap! particle conj {:velocity (vec/add (particle-velocity particle)
                                             [+player-accel+ 0])}))
  
  (when (brain/state? brain :left)
    (swap! particle conj {:velocity (vec/add (particle-velocity particle)
                                             [(- +player-accel+) 0])}))
  (if (supported-by-map @*current-map* (to-rect player))
    ;; on the ground, check for jump
    (when (and (is-cool? particle :jump-timer) (brain/state? brain :up))
      (cooldown-start particle 0.2 :jump-timer)
      (play-sound :jump)
      (swap! particle conj {:velocity [(nth (particle-velocity particle) 0)
                                       (- +jump-speed+)]}))
    
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
  (let [new-pos (vec/add
                 (particle-position particle)
                 (move-check-map-collision
                  @*current-map*
                  (vec/scale (particle-velocity particle)
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
                      true))))]
    (swap! particle conj {:position new-pos})))

(defn timer-value [mutable place]
  (or (place @mutable) 0))

(defn timer-tick [mutable place]
  (let [last-value (timer-value mutable place)]
    (swap! mutable conj {place (inc last-value)})))

(defn timer-scaled [mutable place scale]
  (Math/floor (/ (timer-value mutable place) scale)))

(defrecord Player [particle brain]
  Rectable
  (to-rect [player]
    (let [[x y] (:position @particle)]
      [(+ x 0.1) (+ y 0.3) 0.8 0.7]))

  Tickable
  (tick [player]
    (cooldown-tick particle)
    (timer-tick particle :walk-timer)
    
    ;; record the last directon of motion so we can draw our sprite
    ;; facing that way
    (cond
     (brain/state? brain :left)
     (swap! particle conj {:direction :left})

     (brain/state? brain :right)
     (swap! particle conj {:direction :right}))

    ;; mark ourselves walking or not-walking for animation
    (if (or (brain/state? brain :left) (brain/state? brain :right))
      (swap! particle conj {:walking true})
      (swap! particle conj {:walking false}))

    ;; reset cooldown if no command keys are down
    (when (not (or (brain/state? brain :down) (brain/state? brain :space)))
      (swap! particle conj {:cooldown 0}))

    ;; see if we're trying to use something
    (when (and (is-cool? particle) (brain/state? brain :space))
      (use-thing (first @*bag*) player)
      (cooldown-start particle .3))

    ;; cycle after our cooldown is done
    (when (and (is-cool? particle) (brain/state? brain :down))
      (reset! *bag* (conj (into [] (rest @*bag*)) (first @*bag*)))
      (cooldown-start particle .3))
    
    ;; look around our neighboring rects for collectables
    (map/with-objects-in-rect @*current-map* (to-rect player)
      (fn [obj]
        (when (and (collectable? obj)
                   (rect/intersect (to-rect obj) (to-rect player)))
          (collect obj))))
    
    (player-movement player particle brain)))

(defn walk-frame [direction frame]
  (let [frameset (if (= direction :left)
                   [:left-walk1 :left :left-walk2 :left]
                   [:right-walk1 :right :right-walk2 :right])]
    (frameset (mod frame 4))))

(defn draw-player-entity [ctx p]
  (let [particle @(:particle p)
        direction (or (:direction particle) :right)
        sprite-key (cond
                    (not (collidable-underneath? (to-rect *player*)))
                    (if (= direction :left)
                      :fall-left
                      :fall-right)

                    (:walking particle)
                    (walk-frame direction (timer-scaled (:particle p) :walk-timer 4))

                    :else
                    direction)
        sprite (*player-sprite* sprite-key)]
    (draw-sprite ctx (:image sprite) (:position particle))))


(def +viewport-spring-constant+ 60)
(def +viewport-drag-coefficient+ 2)
(def +viewport-max-displacement+ 2)

(def *viewport* nil)

(defn get-recorded-brain []
  (brain/RecordedBrain.
   (apply array recorded/+quick-run+)
   (atom nil)
   (atom nil)))

(defn add-level-items [level]
  (doseq [[kind instances] (level :items)]
    (doseq [inst instances]
      (add-collectable kind inst)))

  ;; fill our bag
  (reset! *bag* [])
  (doseq [item (level :initial-bag)]
    (add-to-bag (spawn-item item)))

  ;; add the overlays
  (reset! *overlay-entities*
          (map (fn [[key pos]]
                 (OverlayImage. pos (-> @*overlay-assets* key :image)))
               (:overlays level)))

  (level-specific-setup level))

(defn setup-world [player-brain callback]
  (empty-bag)
  (clear-entities @*current-map*)
  (reset! *keys-collected* 0)
  (reset! *bricks-destroyed* 0)
  
  (set!
   *player*
   (Player.
    (atom
     {:mass 5
      :position ((current-level) :player)
      :velocity [0 0]
      })
    player-brain))
  (add-entity @*current-map* player-brain)
  
  (set!
   *viewport*
   (Viewport.
    [20 15]
    (atom
     {:mass 1
      :position ((current-level) :player)
      :velocity [0 0]
      
      ;; try to keep the player character basically centered
      :force-generators
      [(fn [p] (spring-force (vec/sub (:position @(:particle *player*))
                                      (vec/sub (rect/center (viewport-rect))
                                               [0 2]))
                             +viewport-max-displacement+
                             +viewport-spring-constant+))
       (drag-force-generator +viewport-drag-coefficient+)
       
       ;; bring to rest if we're not moving very fast
       (fn [p]
         (let [spd (vec/mag (:velocity p))
               drag-dir (vec/negate (vec/unit (:velocity p)))]
           (vec/scale drag-dir (* spd 0.3))))]})))

  (set-display-and-viewport *canvas* [640 480] #(to-rect *viewport*))

  (reset! *kind-totals* {})
  (add-level-items (current-level))

  (add-entity @*current-map* *viewport*)
  (add-entity @*current-map* *player*)
  (add-entity @*current-map* (Bag. *bag*))
  
  (set! *game-timer* (Timer. (atom {})))
  (add-entity @*current-map* *game-timer*)
  (reset-tick-clock)
  
  (callback))

(defn- service-sound []
  (.loop jukebox.Manager))

(defn prepare-sound []
  (let [sounds {:resources ["sounds/music2.ogg"
                            "sounds/music2.mp3"]
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

        ;; disable music in debug mode
        sounds (if (not *debug-mode*)
                 (conj sounds {:autoplay "bg-music"})
                 sounds)
        
        mgrconfig {"useGameLoop" true}]
    (set! jukebox.Manager (jukebox.Manager. (utils/clj->js mgrconfig)))
    (set! *media-player* (jukebox.Player. (utils/clj->js sounds)))
    (timer/periodic 16 service-sound)))

(defn draw-world [ticks stable-state]
  ;; only draw if we actually ticked
  (when (> ticks 0)
    (let [ctx (gfx/context (display))
          [vx vy _ _] (viewport-rect)]
      (gfx/clear (display))

      (when *backdrop*
        (.drawImage ctx *backdrop* (- 0 50 (* 13 vx)) (- 0 10 (* 13 vy))))
      
      (map/draw ctx @*current-map* (viewport-rect)
                showoff.showoff.*tile-in-world-dims*)
      (draw-entities @*current-map* ctx)

      ;; draw the hitrect
      ;;(filled-rect ctx (to-rect *player*) (color [255 0 255]))

      ;; draw the map rects
      (comment
        (doseq [idx (map/rect->idxs-all @*current-map* (to-rect *player*))]
          (filled-rect ctx (map/idx->rect @*current-map* idx) (color [255 0 255]))))
      
      (draw-player-entity ctx *player*)

      ;; draw overlays
      (doseq [overlay @*overlay-entities*]
        (when (rect/intersect (viewport-rect) (to-rect overlay))
          (draw overlay ctx)))
      
      ;; draw the hud
      (let [[w h] (gfx/img-dims *hud*)
            key-img (:image (:key *collectables*))]
        (.drawImage ctx *hud* 0 0 w h 0 0 640 480)
        (.drawImage ctx key-img (* 2 73) (* 2 218))
        (draw-text ctx *hud-font* (str @*keys-collected*) [180 444])

        (.drawImage ctx *money-icon* (* 2 114) (* 2 218))
        (draw-text ctx *hud-font* (str "$" (* 1000 @*bricks-destroyed*)) [268 444]))
      
      ;; draw whatever is in the front of the bag
      (let [item (first @*bag*)]
        (.drawImage ctx (icon item) 548 436))
      
      (draw-timer ctx *game-timer*)))

  (when *debug-mode*
   (set! (.-innerHTML (utils/by-id "console")) (pr-str (input/state))))

  ;; next state
  (cond
   (= @*keys-collected* (kind-total :key))
   :finished

   :else
   stable-state))


(defn any-keys-pressed? []
  (not (empty? (input/state))))

(def *scorescreen-cooldown* (atom {}))

(defn prepare-finished-screen [callback]
  (cooldown-start *scorescreen-cooldown* 1)
  (cycle-next-level)
  (callback))

(defn finished-screen [ticks]
  (let [ctx (gfx/context (display))
        key-img (:image (:key *collectables*))]
    (.drawImage ctx *base-dialog* 0 0)
    (draw-text-centered ctx *hud-font* "Amazing!!" [320 100])
    (.drawImage ctx key-img 216 150)
    (draw-text ctx *hud-font* (utils/format "All %d found" (kind-total :key)) [264 158])
    (.drawImage ctx *money-icon* 216 182)
    (draw-text ctx *hud-font* (utils/format "$%d" (* 1000 @*bricks-destroyed*)) [264 190])
    (draw-text ctx *hud-font* "of damage done" [264 222])
    
    (draw-text ctx *hud-font* "Press a Key" [264 286]))
  
  (cooldown-tick *scorescreen-cooldown*)
  
  (if (and (is-cool? *scorescreen-cooldown*) (any-keys-pressed?))
    :load-level
    :finished))

(defn window-params []
  (let [qdata (-> (uri/parse (.-location js/window))
                  (.getQueryData))
        qkeys (.getKeys qdata)
        qvals (map #(.get qdata %) qkeys)]
    (zipmap qkeys qvals)))

(def *game-states*
  {:start
   {:setup with-persistent-assets
    :after-ticks (fn [] :load-level)}

   :load-level
   {:setup with-level-assets
    :after-ticks (fn []
                   (if (> (count (window-params)) 0)
                     :recorded-game
                     :game))}
   
   :game
   {:setup (utils/curry setup-world (brain/KeyboardBrain.))
    :after-ticks #(draw-world % :game)}

   :recorded-game
   {:setup (utils/curry setup-world (get-recorded-brain))
    :after-ticks #(draw-world % :recorded-game)}
   
   :finished
   {:setup prepare-finished-screen
    :after-ticks finished-screen}
   })

(defn ^:export game []
  (let [screen-size [640 480]
        canvas (gfx/make-canvas screen-size)]
    
    (utils/append-child (utils/by-id "content") canvas)
    (set! *canvas* canvas)
    (set-display-and-viewport *canvas* [640 480] #(vector 0 0 20 15))
    
    (input/prepare input/standard-remapper)
    (prepare-sound)
    
    (states/game-loop *game-states*)))

(defprotocol Brush
  (brush-apply [brush position])
  (brush-cursor [brush]))

(defrecord FistBrush []
  Brush
  (brush-apply [fist position]
    ;; announce the intent to erase anything here
    (let [[mx my] position
          found-object (atom false)]
      (map/with-objects-in-rect @*current-map* [mx my 1 1]
        (fn [obj]
          (remove-entity @*current-map* obj)
          (event/dispatch-event (event/make :remove-item obj))
          (reset! found-object true)))
      
      ;; see if we can find an overlay to remove instead
      (when-not @found-object
        (doseq [overlay @*overlay-entities*]
          (when (rect/intersect (to-rect overlay) [mx my 1 1])
            (reset! *overlay-entities*
                    (remove #(= overlay %) @*overlay-entities*))
            (event/dispatch-event (event/make :remove-overlay overlay)))))))

  (brush-cursor [fist]
    (-> *collectables* :fist :image)))


(defrecord ItemBrush [kind]
  Brush
  (brush-apply [item position]
    (add-collectable kind position)
    
    (event/dispatch-event
     (event/make :add-item {:kind kind :position position})))

  (brush-cursor [item]
    (-> *collectables* kind :image)))

(defrecord OverlayBrush [kind]
  Brush
  (brush-apply [overlay position]
    (swap! *overlay-entities* conj
           (OverlayImage. position (-> @*overlay-assets* kind :image)))
    (event/dispatch-event
     (event/make :add-overlay {:kind kind :position position})))

  (brush-cursor [overlay]
    (-> @*overlay-assets* kind :image)))

(def *current-brush* (atom {:div nil :brush (FistBrush.)}))

(def *mouse-position* [0 0])

(defn editor-mousemoved [ge]
  (let [x (.-offsetX ge)
        y (.-offsetY ge)]
    
    (set! *mouse-position* [x y])))

(defn mouse-position []
  (let [[mx my] (inverse-transform-position *mouse-position*)]
    [(Math/floor mx) (Math/floor my)]))

(defn change-brush-highlight [new-div]
  (when (:div @*current-brush*)
    (utils/remove-class (:div @*current-brush*) "brush-selected"))

  (utils/set-class new-div "brush-selected"))

(defn select-brush [brush brush-div]
  (change-brush-highlight brush-div)
  (reset! *current-brush* {:div brush-div :brush brush}))

(defn- add-tool [kind]
  (let [brush (ItemBrush. kind)
        box (utils/by-id "tools")
        brush-div (utils/div-with-class "brush" (brush-cursor brush))]
    (utils/append-child box brush-div)
    (gevents/listen brush-div "click" #(select-brush brush brush-div))))

(defn- add-overlay [kind]
  (let [brush (OverlayBrush. kind)
        box (utils/by-id "overlays")
        brush-div (utils/div-with-class "brush" (brush-cursor brush))]
    (utils/append-child box brush-div)
    (gevents/listen brush-div "click" #(select-brush brush brush-div))))

(defrecord EditorViewport [pos brain]
  Rectable
  (to-rect [view]
    (let [[x y] @pos]
      [x y 20 15]))

  Tickable
  (tick [view]
    (when (brain/state? brain :left)
      (let [[x y] @pos]
        (reset! pos [(dec x) y])))

    (when (brain/state? brain :right)
      (let [[x y] @pos]
        (reset! pos [(inc x) y])))

    (when (brain/state? brain :up)
      (let [[x y] @pos]
        (reset! pos [x (dec y)])))

    (when (brain/state? brain :down)
      (let [[x y] @pos]
        (reset! pos [x (inc y)]))))

  MapIndexed
  (map-indexed? [view] false))

(defn editor-mouseclicked [ge]
  (brush-apply (:brush @*current-brush*) (mouse-position)))

(defn setup-editor [callback]
  ;; set up the display
  (add-level-items (current-level))

  ;; set up our pallettes
  (doseq [tool (keys *collectables*)]
    (add-tool tool))

  (doseq [overlay (keys @*overlay-assets*)]
    (add-overlay overlay))

  (let [viewport (EditorViewport. (atom [0 0]) (brain/KeyboardBrain.))]
    (set-display-and-viewport *canvas* [640 480] #(to-rect viewport))
    (add-entity @*current-map* viewport))

  (let [disp (display)]
    (gevents/listen disp "mousemove" editor-mousemoved)
    (gevents/listen disp "click" editor-mouseclicked))

  (callback))

(defn draw-editor [ticks]
  (let [disp (display)
        ctx (gfx/context disp)
        brush-img (brush-cursor (:brush @*current-brush*))]
    
    (gfx/clear disp)
    (map/draw ctx @*current-map* (viewport-rect)
              showoff.showoff.*tile-in-world-dims*)

    (draw-entities @*current-map* ctx)

    ;; draw overlays
    (doseq [overlay @*overlay-entities*]
      (when (rect/intersect (viewport-rect) (to-rect overlay))
        (draw overlay ctx)))

    ;; draw the cursor
    (draw-sprite ctx brush-img (mouse-position))
    
    :editor))

(def *editor-states*
  {:start
   {:setup with-persistent-assets
    :after-ticks (fn [] :load-level)}

   :load-level
   {:setup with-level-assets
    :after-ticks (fn [] :editor)}
   
   :editor
   {:setup setup-editor
    :after-ticks draw-editor}
   })

(defn ^:export map-editor []
  (let [screen-size [640 480]
        canvas (gfx/make-canvas screen-size)]
    
    (utils/append-child (utils/by-id "content") canvas)
    (set! *canvas* canvas)
    (set-display-and-viewport *canvas* [640 480] #(vector 0 0 20 15))
    
    (input/prepare input/standard-remapper)
    
    (states/game-loop *editor-states*)

    (let [out (utils/by-id "output")
          map-data (atom (current-level))]
      
      (event/listen
       :add-item
       (fn [spec]
         (let [items (:items @map-data)
               kind (:kind spec)
               positions (kind items)
               positions (into [] (conj positions (:position spec)))
               items (conj items {kind positions})]
           (swap! map-data conj {:items items})
           (event/dispatch-event
            (event/make :level-updated @map-data)))))

      (event/listen
       :add-overlay
       (fn [spec]
         (let [overlays (:overlays @map-data)
               kind (:kind spec)
               overlays (conj overlays [kind (:position spec)])]
           (swap! map-data conj {:overlays overlays})
           (event/dispatch-event
            (event/make :level-updated @map-data)))))

      (event/listen
       :remove-item
       (fn [obj]
         (let [items (:items @map-data)
               kind (item-key obj)
               position (item-position obj)
               positions (into [] (remove #(= % position) (kind items)))
               items (conj items {kind positions})]
           (swap! map-data conj {:items items})
           (event/dispatch-event
            (event/make :level-updated @map-data)))))

      (event/listen
       :level-updated
       (fn [level]
         (dom/setTextContent out (pr-str level)))))))

(def *brain-recorder* nil)

(defn ^:export record-player []
  (when *brain-recorder*
    (remove-entity @*current-map* *brain-recorder*))
  
  (let [brain (:brain *player*)
        recorder (brain/BrainRecorder. brain (atom []))]
    (add-entity @*current-map* recorder)
    (set! *brain-recorder* recorder)))

(defn ^:export dump-recording []
  (.log js/console (pr-str @(:recording *brain-recorder*))))
