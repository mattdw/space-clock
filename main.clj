(ns com.culturethree.SpaceClock
	(:import (javax.swing JFrame JPanel)
	         (java.awt Color Graphics2D Dimension RenderingHints BasicStroke GradientPaint GraphicsEnvironment)
	         (java.awt.image BufferedImage)
	         (java.awt.geom Arc2D Arc2D$Double Ellipse2D$Float)
                 (com.sun.awt AWTUtilities AWTUtilities$Translucency)
                 (java.util Calendar)))

;;
;; CONSTANTS – MODIFY TO TASTES

(def *default-size* 400)
(let [size-arg (first (filter #(.startsWith % "-s=") *command-line-args*))
      size (if size-arg (second (.split size-arg "=")) *default-size*)]
  (def *size* (int (new Integer size))))

(def *padding* #(/ % 10))

(def *color-minutes* (new Color 50 50 60))
(def *color-hours* (new GradientPaint
                        0 0 (new Color 150 150 155) 
                        *size* 0 (new Color 200 200 210)))
(def *color-seconds* (new Color 200 50 50))

(def *panel-background-color* (new Color 250 250 252 255))
(def *background-color* *panel-background-color*)
(def *divider-color* *background-color*)

;; Diameter constants should be provided as a function which will be
;; passed the current maximum diameter. The defaults here represent
;; a proportion of the diameter.
(def *diam-minutes* #(* % 0.97))
(def *diam-hours* #(* % 0.9))
(def *diam-seconds* identity)

;; END CONSTANTS
;;

(defstruct clock :hours :minutes :seconds)
(def CLOCK (atom (struct clock 0 0 0)))
(def LAST-DIMS (ref [0 0]))
(def OVERLAY (ref nil))

(defn is-pm?
  [{h :hours}]
  (> h 12))

(defn even-minute?
  [{m :minutes}]
  (even? (quot m 1)))

(defn even-hour?
  [{h :hours}]
  (even? (quot h 1)))

(defn polar-to-cartesian
  "convert polar coordinates to their cartesian equivalent"
  [r theta]
  (let [x (* r (Math/cos theta))
        y (* r (Math/sin theta))]
    [x y]))

(defn endpoint-for-tick
  "returns (in radians) the endpoints for a tick mark from 1-12"
  [diam n]
  (let [pi2 (* Math/PI 2)
        ratio (/ n 12)
        theta (* ratio pi2)
        ; tick marks start just outside the diameter
        r-start (+ diam (* diam 0.02))
        ; and have a length proportional to their number value
        r-end (+ (* ratio diam 0.15) r-start)]
    [(rem (+ theta (* pi2 0.75)) pi2) r-start r-end]))

(defn calc-arc
  "Return start and extent of arc for component"
  [current total even?]
  (let [percent (/ current total)
        degrees (* percent 360)]
    (if even?
      ; if it's even, the leading edge is moving
      (let [start 0
            extent degrees]
        [(- start) (- extent)])
      ; if it's odd, the trailing edge is moving
      (let [start degrees
            extent (- 360 degrees)]
        [(- start) (- extent)]))))

(defn make-arc
  "Return a new Arc2D, calculating correct location around a center point"
  ([[w h] diam start extent cap]
    (let [left (/ (- w diam) 2)
          top  (/ (- h diam) 2)
          start1 (rem (+ 90 start) 360)
          extent1   (rem (+ 90 extent) 360)]
      (new Arc2D$Double left top diam diam start1 extent cap)))
  ([dims diam start extent]
    (make-arc dims diam start extent Arc2D/PIE)))

(defn make-circle
  "Return a new Ellipse 2D, calculating correct location as per make-arc"
  [[w h] diam]
  (let [left (/ (- w diam) 2)
        top  (/ (- h diam) 2)]
    (new Ellipse2D$Float left top diam diam)))

(defn draw-overlay
  "Draw the clock decorations – section dividers, center point, tick marks."
  [#^Graphics2D g [width height :as dims]]
  (let [;; sizes
        size               (min width height)
        padding            (*padding* size)
        d-max              (- size (* 2 padding))
        diam-hours         (*diam-hours* d-max)
        diam-minutes       (*diam-minutes* d-max)
        diam-seconds       (*diam-seconds* d-max)
        ;; decorative arcs
        center-dot-w       (make-circle dims (* d-max 0.04))
        center-dot-b       (make-circle dims (* d-max 0.015))]
    (.setRenderingHint g
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (doto g
      (.setStroke (new BasicStroke (* d-max 0.005)))
      (.setColor (new Color 0 0 0 180)))
    (let [c-x (/ width 2)
          c-y (/ height 2)
          r-max (/ d-max 2)]
      (doseq [n (range 1 13)]
        (let [[theta r1 r2] (endpoint-for-tick r-max n)
              [x1 y1] (polar-to-cartesian r1 theta)
              [x2 y2] (polar-to-cartesian r2 theta)]
          (.drawLine g (+ x1 c-x) (+ y1 c-y) (+ x2 c-x) (+ y2 c-y)))))
    (doseq [[arc color] [[center-dot-w *divider-color*]
                         [center-dot-b (Color/darkGray)]]]
      (.setColor g color)
      (.fill g arc))
    (.dispose g)))

(defn get-overlay
  "Retrieve from cache (redrawing if necessary) and draw the overlay."
  [#^Graphics2D g [width height :as dims]]
  (when (not (and (= dims @LAST-DIMS) @OVERLAY))
      (let [new-image (new BufferedImage width height BufferedImage/TYPE_INT_ARGB)
            new-context (.getGraphics new-image)]
        (draw-overlay new-context dims)
        (dosync
          (ref-set LAST-DIMS dims)
          (ref-set OVERLAY new-image))
        (.dispose new-context)))
  (.drawImage g @OVERLAY 0 0 width height nil nil))

(defn draw-clock
  "Draw the clock, as a series of overlaid concentric circles and arcs."
  [#^Graphics2D g [width height :as dims] {:keys [hours minutes seconds] :as clock}]
  (let [; sizes
        size               (min width height)
        padding            (*padding* size)
        d-max              (- size (* 2 padding))
        diam-hours         (*diam-hours* d-max)
        diam-minutes       (*diam-minutes* d-max)
        diam-seconds       (*diam-seconds* d-max)
        ; flags
        even-hour          (even-hour? clock)
        even-minute        (even-minute? clock)
        twelve-hours       (rem hours 12)
        is-pm              (is-pm? clock)
        ; arcs
        h-arc              (make-circle dims diam-hours)
        [h-start h-extent] (calc-arc twelve-hours 12 is-pm)
        h-arc-cur          (make-arc dims (* diam-hours 0.984) h-start h-extent)
        m-arc              (make-circle dims diam-minutes)
        [m-start m-extent] (calc-arc minutes 60 even-hour)
        m-arc-cur          (make-arc dims (* diam-minutes 0.988) m-start m-extent)
        s-arc              (make-circle dims diam-seconds)
        [s-start s-extent] (calc-arc seconds 60 even-minute)
        s-arc-cur          (make-arc dims (* diam-seconds 0.988) s-start s-extent)
        ]
    (.setRenderingHint g
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (.clearRect g 0 0 width height)
    (doseq [[arc color] [
                         [s-arc     *background-color*]
                         [s-arc-cur *color-seconds*]
                         [m-arc     *background-color*]
                         [m-arc-cur *color-minutes*]
                         [h-arc     *background-color*]
                         [h-arc-cur *color-hours*]
                        ]]
      (doto g
        (.setPaint color)
        (.fill arc)))
    (get-overlay g dims)
    (.dispose g)))

(defn draw-panel
  "Call render function with current dimensions and time. Keeps draw-clock pure."
  [#^JPanel p #^Graphics2D g clock]
  (let [dims [(.getWidth p) (.getHeight p)]]
    (draw-clock g dims @clock)))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (draw-panel panel g CLOCK)))
                 (.setPreferredSize (new Dimension *size* *size*))
                 (.setBackground *panel-background-color*)
                 (.setMinimumSize (new Dimension 150 150))
                 (.setMaximumSize (new Dimension 2000 2000))))

(let [env (GraphicsEnvironment/getLocalGraphicsEnvironment)
      devices (.getScreenDevices env)
      configs (mapcat #(.getConfigurations %) devices)
      new_conf (first (filter #(AWTUtilities/isTranslucencyCapable %) configs))]
  (println "found configs:" (count configs))
  (def gc_conf new_conf))

(def frame (doto (new JFrame "Clock" gc_conf)
                 (.add panel)
                 (.setUndecorated true)
                 ;(AWTUtilities/setWindowOpaque false)
                 (.setBackground (new Color 0 0 0 0))
                 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                 .pack .show))

(defn main-loop
  []
  (let [d (Calendar/getInstance)
        h (.get d (Calendar/HOUR_OF_DAY))
        m (.get d (Calendar/MINUTE))
        s (.get d (Calendar/SECOND))
        ms (.get d (Calendar/MILLISECOND))
        s-floating (+ s (/ ms 1000))
        m-floating (+ m (/ s-floating 60))
        h-floating (+ h (/ m-floating 60))]
    (swap! CLOCK assoc :hours h-floating :minutes m-floating :seconds s-floating)
    (.repaint #^JPanel panel)
    (let [new-d (Calendar/getInstance)
	  new-ms (.get new-d (Calendar/MILLISECOND))]
      (Thread/sleep (- 1000 new-ms)))
    (recur)))

(main-loop)
