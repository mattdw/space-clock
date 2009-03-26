(ns com.culturethree.Timer
	(:import (javax.swing JFrame JPanel JButton BorderFactory)
	         (java.awt Color Graphics2D Dimension RenderingHints BorderLayout)
	         (java.awt.geom Arc2D Arc2D$Float)
           (java.util Calendar)))


(def *padding* 30)
(def *size* 500)
(def *window-size* (+ *size* (* 2 *padding*)))

(defstruct clock :hours :minutes :seconds)
(def CLOCK (atom (struct clock 11 22 33)))

(defn angle-for-measure
  "Return degrees out of a circle for current part"
  [current total]
  (let [percent (/ current total)
        degrees (- (* percent 360))]
    degrees))

(defn make-arc
  [[w h] diam start extent]
  (let [left (/ (- w diam) 2)
        top  (/ (- h diam) 2)
        start1 (rem (+ 90 start) 360)
        extent1   (rem (+ 90 extent) 360)]
    (new Arc2D$Float left top diam diam start1 extent Arc2D/PIE)))

(defn draw-clock
  [p #^Graphics2D g [width height :as dims] {:keys [hours minutes seconds]}]
  (let [d (- (min width height) (* 2 *padding*))
        diam-minutes (/ d 2)
        diam-seconds (/ d 4)
        h-arc     (make-arc dims d 0 360)
        h-arc-cur (make-arc dims (- d 2) 0 (angle-for-measure hours 12))
        m-arc     (make-arc dims diam-minutes 0 360)
        m-arc-cur (make-arc dims (- diam-minutes 2) 0 (angle-for-measure minutes 60))
        s-arc     (make-arc dims diam-seconds 0 360)
        s-arc-cur (make-arc dims (- diam-seconds 2) 0 (angle-for-measure seconds 60))]
    (.setRenderingHint g
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    ;(.setColor g (. Color black))
    ;(.fillRect g 0 0 width height)
    (doseq [[arc colour] [
                          [h-arc     (. Color lightGray)]
                          [h-arc-cur (. Color white)]
                          [m-arc     (. Color gray)]
                          [m-arc-cur (. Color white)]
                          [s-arc     (new Color (float 0.5) (float 0.4) (float 0.4))]
                          [s-arc-cur (. Color white)]
                        ]]
      (doto g
        (.setColor colour)
        (.draw arc)
        (.fill arc)))
    (.dispose g)))

(defn draw-panel
 [#^JPanel p #^Graphics2D g clock]
 (let [dims [(.getWidth p) (.getHeight p)]]
   (draw-clock p g dims @clock)))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (draw-panel panel g CLOCK)))
                 (.setPreferredSize (new Dimension *window-size* *window-size*))
                 (.setMinimumSize (new Dimension 50 50))
                 (.setMaximumSize (new Dimension 2000 2000))))

(def frame (doto (new JFrame "Timer")
                 (.add panel)
                 (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                 .pack .show))

(defn timer-loop
  []
  (let [d (Calendar/getInstance)
        h (rem (.get d (Calendar/HOUR)) 12)
        m (.get d (Calendar/MINUTE))
        s (.get d (Calendar/SECOND))
        ms (.get d (Calendar/MILLISECOND))
        s-floating (+ s (/ ms 1000))
        m-floating (+ m (/ s-floating 60))
        h-floating (+ h (/ m-floating 60))]
    (swap! CLOCK assoc :hours h-floating :minutes m-floating :seconds s-floating)
    (.repaint #^JPanel panel)
    (Thread/sleep 100)
    (recur)))

(timer-loop)
