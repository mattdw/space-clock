(ns com.culturethree.Timer
	(:import (javax.swing JFrame JPanel JButton BorderFactory)
	         (java.awt Color Graphics2D Dimension RenderingHints BasicStroke)
	         (java.awt.geom Arc2D Arc2D$Double)
           (java.util Calendar)))


(def *size* 400)
(def *padding* #(/ % 10))

(def *color-minutes* (new Color 50 50 60))
(def *color-hours* (new Color 170 170 180))
(def *color-seconds* (new Color 200 50 50))

(def *panel-background-color* (new Color 253 253 253))
(def *background-color* *panel-background-color*)
(def *divider-color* Color/white)

(def *diam-minutes* #(* % 0.975))
(def *diam-hours* #(* % 0.7))
(def *diam-seconds* identity)

(defstruct clock :hours :minutes :seconds)
(def CLOCK (atom (struct clock 11 22 33)))

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
  "returns (in radians) the endpoints for a tick mark from 0-11"
  [diam n]
  (let [pi2 (* Math/PI 2)
        ratio (/ n 12)
        theta (* ratio pi2)
        r-start (+ diam (* diam 0.02))
        r-end (+ (* ratio diam 0.15) r-start)]
    [(rem (+ theta (* pi2 0.75)) pi2) r-start r-end]))

(defn calc-arc
  "Return start and extent of arc for component"
  [current total even?]
  (let [percent (/ current total)
        degrees (* percent 360)]
    (if (not even?)
      (let [start 0
            extent degrees]
        [(- start) (- extent)])
      (let [start degrees
            extent (- 360 degrees)]
        [(- start) (- extent)]))))

(defn make-arc
  ([[w h] diam start extent cap]
    (let [left (/ (- w diam) 2)
          top  (/ (- h diam) 2)
          start1 (rem (+ 90 start) 360)
          extent1   (rem (+ 90 extent) 360)]
      (new Arc2D$Double left top diam diam start1 extent cap)))
  ([dims diam start extent]
    (make-arc dims diam start extent Arc2D/PIE)))

(defn draw-clock
  [p #^Graphics2D g [width height :as dims] {:keys [hours minutes seconds] :as clock}]
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
        h-arc              (make-arc dims diam-hours 0 360)
        [h-start h-extent] (calc-arc twelve-hours 12 is-pm)
        h-arc-cur          (make-arc dims diam-hours h-start h-extent)
        m-arc              (make-arc dims diam-minutes 0 360)
        [m-start m-extent] (calc-arc minutes 60 even-hour)
        m-arc-cur          (make-arc dims diam-minutes m-start m-extent)
        s-arc              (make-arc dims diam-seconds 0 360)
        [s-start s-extent] (calc-arc seconds 60 even-minute)
        s-arc-cur          (make-arc dims diam-seconds s-start s-extent)
        ; decorative arcs
        center-dot-w       (make-arc dims (* d-max 0.07) 0 360)
        center-dot-b       (make-arc dims (* d-max 0.01) 0 360)
        div1               (make-arc dims diam-hours 0 360 Arc2D/OPEN)
        div2               (make-arc dims diam-minutes 0 360 Arc2D/OPEN)
        outer              (make-arc dims d-max 0 360 Arc2D/OPEN)
        ]
    (.setRenderingHint g
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    
    (.clearRect g 0 0 width height)
    (doseq [[arc color] [
                         [s-arc     *color-seconds*]
                         [s-arc-cur *background-color*]
                         [m-arc     *color-minutes*]
                         [m-arc-cur *background-color*]
                         [h-arc     *color-hours*]
                         [h-arc-cur *background-color*]
                         [center-dot-w *divider-color*]
                         [center-dot-b (Color/darkGray)]
                        ]]
      (doto g
        (.setColor color)
        (.fill arc)))
    (doto g
      (.setStroke (new BasicStroke 2))
      (.setColor *divider-color*)
      (.draw div1)
      (.draw div2)
      (.draw outer)
      (.setStroke (new BasicStroke 0.5))
      (.setColor (new Color 0 0 0 120)))
    (let [c-x (/ width 2)
          c-y (/ height 2)
          r-max (/ d-max 2)]
      (doseq [n (range 1 13)]
        (let [[theta r1 r2] (endpoint-for-tick r-max n)
              [x1 y1] (polar-to-cartesian r1 theta)
              [x2 y2] (polar-to-cartesian r2 theta)]
          (.drawLine g (+ x1 c-x) (+ y1 c-y) (+ x2 c-x) (+ y2 c-y)))))
    (.dispose g)))

(defn draw-panel
 [#^JPanel p #^Graphics2D g clock]
 (let [dims [(.getWidth p) (.getHeight p)]]
   (draw-clock p g dims @clock)))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (draw-panel panel g CLOCK)))
                 (.setPreferredSize (new Dimension *size* *size*))
                 (.setBackground *panel-background-color*)
                 (.setMinimumSize (new Dimension 150 150))
                 (.setMaximumSize (new Dimension 2000 2000))))

(def frame (doto (new JFrame "Clock")
                 (.add panel)
                 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                 .pack .show))

(defn timer-loop
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
    (Thread/sleep (- 1000 ms))
    (recur)))

(timer-loop)
