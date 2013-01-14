plotting-first-input ???


Facade:

(plot f xlow xhigh)
- Draw the function in a Scheme window.  Return the plot object.
- Control the size of the Scheme window with *scheme-plot-window-x-res*,
  *scheme-plot-window-y-res*
- Control the plot's resolution with *plot-x-res* and *plot-y-res*

(gnuplot f xlow xhigh)
- Draw the function in a fresh gnuplot window.  Return the plot object.
  - with visibly shows the plot in a Scheme window while it's being refined
- TODO pick an interface for gnuplot control strings
  - global variables to hold defaults if parameters not supplied?


Of independent interest:

(gnuplot-alist alist ...)

(gnuplot-histogram-alist alist ...)
- TODO Maybe unrelease this and keep it in my own files?
- I think this is implementable in terms of gnuplot-alist now,
  using the gnuplot-prefix argument


Interactive manipulation:

(plot f xlow xhigh 'invisibly)
- Do the adaptive point selection, but produce no output.  Return the
  plot object.

(plot-draw! plot)
- Draw the plot in a Scheme window.  (Does not autorefine)

plot-zoom!, plot-zoom-x!, plot-zoom-y!
- Zoom the plot view (in or out) and refine
- The semantics of omitting bounds along the x dimension are complicated:
  They constitute a request to test the function at -1 and 1, but then to
  clip the plot output to include all available data.

plot-resolve!
- set the plot's resolution and refine
- The semantics of the plot's resolution are that it determines the
  size of features the refinement will try to explore.  For best
  visual results, this should match or exceed the resolution of the
  graphic where the plot will be drawn.  For fastest computation, this
  should be as small as possible.  Only the product matters for
  locally quadratic refinement.

(plot-stop-drawing! plot)
- Get rid of the Scheme window (it will not be recreated automatically)

(plot-gnu! plot)
- Draw the (current state of) the plot in a fresh gnuplot window.  (Does not
  autorefine)


Interactive querying:

plot-xlow plot-xhigh plot-ylow plot-yhigh plot-dimensions

plot-xresolution ? plot-yresolution ?

(plot-known-points-alist plot)
- extract the known points as a list of x-y pairs

(plot-relevant-points-alist plot)
- extract the known points filtered by the current viewport

TODO discover the size of the discrepancy
- Maybe this is just a unit testing utility?


Manipulate without autorefinement:

(start-plot f xlow xhigh)
- Create a plot, but do not make a Scheme window or invoke the
  function to be plotted.

plot-resize!, plot-resize-x!, plot-resize-y!
- Zoom the plot view without refining

plot-new-resolution!
- set the plot's resolution without triggering refinement

plot-refine!
- manually invoke adaptive refinement

plot-uniform-refine!, plot-uniform-refine-x!, plot-uniform-refine-y!
- manually refine a plot in the x or y dimension or both with uniform spacing
  - the spacing is determined by the plot size and resolution
  - uniform-refine-y does not loop until done because if f had a
    visible discontinuity it would never be done.

TODO document plot-dim-refine! as a way to refine a plot to a
resolution other than the one the plot is targeting?
