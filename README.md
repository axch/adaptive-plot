plotting-first-input ???

TODO Control the size of the discrepancy that is considered acceptable
- maybe in the facade, certainly in the interactive manipulations


Facade:

(plot f xlow xhigh)
- Draw the function in a Scheme window.  Return the plot object and set last-plot.

(gnuplot f xlow xhigh)
- Draw the function in a fresh gnuplot window.  Set last-plot and
  return the plot object.
- TODO option to show the plot in a Scheme window while it's being refined
  - If so, the Scheme window should be closed before the gnuplot
    window appears
- TODO pick an interface for gnuplot control strings

TODO control the resolution of the plot somewhere
- fluids?


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

TODO plot-resize!, plot-resize-x!, plot-resize-y!
- Zoom the plot view (in or out) and refine
- The semantics of omitting bounds along the x dimension are complicated:
  They constitute a request to test the function at -1 and 1, but then to
  clip the plot output to include all available data.

TODO set the plot's resolution and autorefine

(plot-close-window! plot)
- Get rid of the Scheme window (and don't recreate it)

(plot-gnu! plot)
- Draw the (current state of) the plot in a fresh gnuplot window.  (Does not
  autorefine)


Interactive querying:

plot-xlow plot-xhigh plot-ylow plot-yhigh plot-dimensions

plot-xresolution ? plot-yresolution ?

TODO (plot-known-points-alist plot)
- extract the known points

TODO (plot-relevant-points-alist plot)
- extract the known points filtered by the current viewport

TODO (plot-write-points! plot filename)
- write the plot out as a gnuplot-compatible data table

TODO discover the size of the discrepancy


Manipulate without autorefinement:

(start-plot f xlow xhigh)
- Create a plot, but do not make a Scheme window or invoke the
  function to be plotted.

plot-resize!, plot-resize-x!, plot-resize-y!
- Zoom the plot view without refining

TODO set the plot's resolution without triggering refinement

plot-refine!
- manually invoke adaptive refinement

plot-uniform-refine!, plot-uniform-refine-x!, plot-uniform-refine-y!
- manually refine a plot in the x or y dimension or both with uniform spacing
  - the spacing is determined by the plot size and resolution;

TODO document plot-dim-refine! as a way to refine a plot to a
resolution other than the one the plot it targeting?
