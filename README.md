Facade:

(plot f xlow xhigh)
- Draw the function in a Scheme window.  Return the plot object.
- Control the size of the Scheme window with *scheme-plot-window-x-res*,
  *scheme-plot-window-y-res*
- Accepts adverbs
  - 'visibly 'invisibly
  - 'uniformly '(uniformly res) '(x-uniformly xres) '(y-uniformly yres)
  - 'adaptively '(adaptively res^2) '(adaptively xres yres)
  - '(adaptively-with count)
  - '(adaptively-to-with xres yres count)
  - default: visibly, (adaptively 1200 960)
- Control the plot's default resolution with *plot-x-res* and *plot-y-res*

(gnuplot f xlow xhigh)
- Draw the function in a fresh gnuplot window.  Return the plot object.
- accepts additional adverbs
  - '(prefixing str) '(commanding str)


Of independent interest:

(gnuplot-alist alist ...)
- '(prefixing str) '(commanding str)


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

(new-plot f xlow xhigh)
- Create a plot, but do not make a Scheme window or invoke the
  function to be plotted.

plot-resize!, plot-resize-x!, plot-resize-y!
- Zoom the plot view without refining

plot-new-resolution!
- set the plot's resolution without triggering refinement

plot-refine!
- manually invoke adaptive refinement
- accepts refinement adverbs
- does not and should not care about visibility adverbs
