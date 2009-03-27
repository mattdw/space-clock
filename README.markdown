A modernist-style analog clock, written in Clojure.

The clock is read as a normal analog clock, with seconds, minutes, then hours from the outside in. One edge of each arc always points at 12; the other edge is what you read.

![Screenshot of the clock.](http://problemattic.net/media/i/content/clock-screenshot.png)

*This screenshot shows 5:22:20 PM. Seconds in red, minutes light gray, hours dark gray.*

If it's getting dark (i.e. the dark part of the hours segment is growing) it's PM; if the white segment of the hours is growing it's AM.
