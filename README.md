plotting-first-input ???

TODO Control the size of the discrepancy that is considered acceptable
- maybe in the facade, certainly in the interactive manipulations


Facade:

(plot f xlow xhigh)
- Draw the function in a Scheme window.  Return the plot object and set last-plot.
- TODO control the size of the Scheme window, or come up with really good defaults

(gnuplot f xlow xhigh)
- Draw the function in a fresh gnuplot window.  Set last-plot and
  return the plot object.
- TODO option to show the plot in a Scheme window while it's being refined
  - If so, the Scheme window should be closed before the gnuplot
    window appears
  - Control the size of the Scheme window, or come up with really good defaults
- TODO pick an interface for gnuplot control strings
  - global variables to hold defaults if parameters not supplied?

TODO control the resolution of the plot somewhere
- optionals? fluids?


Of independent interest:

(gnuplot-plot-alist alist ...)
- TODO rename to gnuplot-alist

(gnuplot-histogram-alist alist ...)
- TODO Maybe unrelease this and keep it in my own files?
- I think this is implementable in terms of gnuplot-plot-alist now,
  using the gnuplot-prefix argument


Interactive manipulation:

last-plot

(plot-quietly f xlow xhigh)
- Do the adaptive point selection, but produce no output.  Return the
  plot object and set last-plot.

(plot-draw! plot)
- Draw the plot in a Scheme window.  (Does not autorefine)

plot-zoom!, plot-zoom-x!, plot-zoom-y!
- Zoom the plot view (in or out) and refine
- The semantics of omitting bounds along the x dimension are complicated:
  They constitute a request to test the function at -1 and 1, but then to
  clip the plot output to include all available data.

plot-resolve!
- set the plot's resolution and refine

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
resolution other than the one the plot it targeting?
