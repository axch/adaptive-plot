plotting-first-input ???

TODO Control the size of the discrepancy that is considered acceptable
- maybe in the facade, certainly in the interactive manipulations


Facade:

(plot f xlow xhigh)
- Draw the function in a Scheme window.  Return the plot object and set last-plot.

TODO (gnuplot f xlow xhigh)
- Draw the function in a fresh gnuplot window.  To flash a Scheme window
  or not?  In any case the Scheme window should be closed before the gnuplot
  window appears.  Set last-plot and return the plot object.
- Should accept gnuplot control strings

TODO control the resolution of the plot somewhere
- fluids?


Of independent interest:

(gnuplot-plot-alist alist ...)
- TODO rename to gnuplot-alist

(gnuplot-histogram-alist alist ...)
- TODO Maybe keep this private?
- I think this is implementable in terms of gnuplot-plot-alist now,
  using the gnuplot-prefix argument


Interactive manipulation:

last-plot

TODO (plot-quitely f xlow xhigh)
- Do the adaptive point selection, but produce no output.  Return the
  plot object

(plot-draw! plot)
- Draw the plot in a Scheme window.  (Does not autorefine)

TODO plot-resize!, plot-resize-x!, plot-resize-y!
- Zoom the plot view (in or out) and refine

plot-close-window!
- Get rid of the Scheme window (and don't recreate it)

(plot-gnuplot! plot)
- Draw the (current state of) the plot in a fresh gnuplot window.  (Does not
  autorefine)
- TODO maybe call this plot-gnu! ?


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

TODO (plot-start f xlow xhigh)
- Initialize a quiet plot but do not autorefine yet

plot-resize!, plot-resize-x!, plot-resize-y!
- Zoom the plot view without refining

TODO plot-refine-x!, plot-refine-y!
- manually refine a plot in the x or y dimension with uniform spacing
  - the spacing is sort of determined by the plot size and resolution;
    
plot-refine!
- manually invoke adaptive refinement
