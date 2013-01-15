Has this ever happened to you?
==============================

<table>
<tr><td align="center">
<img src="http://web.mit.edu/~axch/www/plotting/oops.png" alt="Bad plot
 from poorly chosen points" title="...">
</td></tr>
<tr><td align="center">
<p style="padding:0 auto"><b>Figure a</b>: A function plotted with 40 data points
evenly spaced along the x axis</p>
</td></tr>
</table>

40 hours spent computing 40 data points, and all the insight you get
into your hard-to-compute function is Figure a?  Wouldn't you rather
have something like Figure b for the same effort?

<table>
<tr><td align="center">
<img src="http://web.mit.edu/~axch/www/plotting/better.png" alt="Good plot
 from well chosen points" title="...">
</td></tr>
<tr><td align="center">
<p style="position:relative; padding:0 100px"><b>Figure b</b>: The same function
plotted with 40 data points with adaptively chosen x coordinates.</p>
</td></tr>
</table>

Enter Adaptive Plot
===================

TODO Examples

Concepts
========

A *plot* of a function is really an ordered collection of (x,y)
points, where the x coordinates were chosen by some mechanism and the
y coordinates were computed by applying the function to those x
coordinates.

The actual pictures are *linear interpolations* -- draw the leftmost
point, draw a straight line from it to the next point, then from there
to the next, etc.  (One could interpolate nonlinearly, for instance
with splines, but the point is to study the function, so I chose
linear interpolation for Adaptive Plot because it is very simple and
easily understood.)  This means that the picture you see is actually a
piecewise-linear approximation of your function.

A plot will look smooth when the line segments it is made of are small
enough compared to the angles they make with each other and the
thickness of the lines drawn on screen that your eye runs them
together into one curve.  In regions where the function being plotted
is almost linear, this can be accomplished with relatively sparse
sampling; in regions of high curvature, denser sampling is required.

Adaptive Plot watches the points that it computes when building a
plot, and, based on the choppiness of the piecewise-linear curve that
they make, estimates whether additional points are necessary and where
they would be most useful.

The *resolution* of a plot is the resolution of the screen on which
the plot is assuming it will be drawn: higher means features will be
more smoothed out (at the cost of computing the value of the function
at more points).  For best visual results, the resolution of the plot
should be at least as high as the resolution of graphic where you will
actually draw it; however, you may wish to draw a low-resolution plot
on a high-resolution graphic to get a fast coarse view of an expensive
function.

Reference Manual
================

Making Simple Things Simple
---------------------------

`(plot f xlow xhigh . adverbs)`

Draw the function `f` in a Scheme window.  The Scheme window is live,
in the sense that new points are added as soon as they are computed
rather than waiting for the plotting process to complete.  The
liveness is mainly useful when `f` is expensive to evaluate.  Return
the plot object.

You can control several aspects of how this is done by supplying
_adverbs_, which are either Scheme symbols or lists that begin with
Scheme symbols (you need to quote them to keep them from being
evaluated) (later adverbs override earlier ones):

- `'invisibly` tells `plot` not to actually draw the Scheme window.
  This is useful if you want to manipulate the resulting plot object
  some other way before looking at the plot.  See also `new-plot`,
  below.

- `'visibly`, in contrast, requests the live Scheme window.  This is
  the default for `plot`, but e.g., `gnuplot`, below, does not show a
  live window by default.

- `'adaptively '(adaptively res^2) '(adaptively xres yres)
  '(adaptively-with count) '(adaptively-to-with xres yres count)
  'x-uniformly '(x-uniformly xres) 'y-uniformly '(y-uniformly yres)
  'uniformly '(uniformly res)` control the actual point selection
  algorithm, discussed below.  The default is `'adaptively`.

In addition, you can control the size of the generated Scheme window
by setting the global variables `*scheme-plot-window-x-res*` and
`*scheme-plot-window-y-res*`, and you can control the plot's default
target resolution by setting the global variables `*plot-x-res*` and
`*plot-y-res*`.

`(gnuplot f xlow xhigh)`

Draw the function `f` in a Gnuplot window.  This operation is
synchronous: `gnuplot` does not return until you close Gnuplot window.
Return the plot object.

By default, `gnuplot` does not draw a Scheme window while `f` is being
plotted, but passing it the `visibly` adverb will make it do so.  The
Scheme window will be closed when the plotting is done, to make room
for the Gnuplot window (since Gnuplot renders plots more
aesthetically).

`gnuplot` accepts all the same adverbs that `plot` does, plus two
more:

- `'(prefixing str)` will insert the given `str` in the Gnuplot
  command stream before the `plot` command that draws the desired
  plot.  Use this, for instance, to ask Gnuplot to write the plot
  to a file.

- `'(commanding str)` will insert the given `str` as an additional
  modifier at the end of the `plot` command that draws the desired
  plot.  Use this, for instance, to give a name to your function to
  appear in the legend.

TODO For example

TODO document existing default gnupolt commands

`(replot plot . adverbs)`, `(regnuplot plot . adverbs)`

Apply new adverbs to an existing plot and redraw (either in a Scheme
window or in Gnuplot, as appropriate).  Previously supplied adverbs
are not remembered, but previously plotted points are.  Adverbs that
indicate plotting with some number of points mean use that many more
points.

Plot Refinement Algorithms
--------------------------

*Parabolic* refinement compares the linear approximation to a locally
quadratic approximation (from sets of three consecutive
already-plotted points) and queries that x coordinate that brings them
closest together.  Parabolic refinement adds one point at a time, in a
greedy fashion: query wherever looks best at the time.  Parabolic
refinement is the default; you can also select it explicitly with the
`adaptively` adverbs, which also lets you control the stopping
condition.

- `'adaptively' selects parabolic refinement until the plot's target
  resolution is reached (default 1200x960).

- `'(adaptively res^2)' selects parabolic refinement until the given
  resolution, in units of total pixels, is reached.  (Parabolic
  refinement works on total area, so the target aspect ratio of the
  plot does not affect it).

- `'(adaptively xres yres)` selects parabolic refinement until
  the given resolution.  Since parabolic refinement works on
  total area, this is equivalent to `(adaptively (* xres yres))`.

- `'(adaptively-with count)` selects parabolic refinement for a fixed
  number of points, regardless of the resolution attained thereby.
  Note that the first 12 points are spaced uniformly along the x-axis
  to make an initial approximation to refine, so to actually benefit
  from parabolic refinement `count` should be more than 12.

- `'(adaptively-to-with xres yres count)` is a combination of the
  previous two: refinement proceeds until either the desired
  resolution is reached or `count` points have been added, and then
  stops.

Arguments to `adaptively-with` and `adaptively-to-with` are optional.
Without the `count`, the latter reduces to `adaptively`; whereas the
former, without the `count`, will refine forever.

*Uniform* refinement along either the x or the y dimension makes sure
that all segments are not too long in the given dimension, by trying
to break any such too-long segment up into the fewest uniform-length
pieces necessary to bring it down to size.  Uniform refinement in the
x dimension is therefore not adaptive at all (because the x
coordinates of new points do not depend on the values of the function
at old points), but uniform refinement in the y dimension is adaptive,
in the sense that it will throw points at regions where the function's
value is changing rapidly.

You can select uniform refinement along one or both dimensions with
the `uniformly` adverbs, as well as control the size it aims for

- `'(x-uniformly xres)` makes the x-lengths of all segments shorter
  than the given resolution (higher resolution means shorter
  segments).

- `'x-uniformly` makes the x-lengths of all segments shorter than the
  plot's x-resolution.  Note that the default for plot resolution is
  chosen with parabolic refinement in mind, so this adverb may be
  surprisingly expensive.

- `'(y-uniformly yres)` tries to make the y-lengths of all segments
  shorter than the given resolution (higher resolution means shorter
  segments).  The attempt is made by breaking any y-longer segment
  uniformly in the x dimension; if the function is nonlinear, not all
  the resulting segments will have y-lengths shorter than the desired
  resolution.  y-uniform refinement is not automatically iterated,
  because if the function has a sufficienly large discontinuity,
  iteration would not terminate.

- `'y-uniformly` tries to make the y-lengths of all segments shorter
  than the plot's y-resolution.

- `'(uniformly res)` does `'(x-uniformly res)` first and then
  `'(y-uniformly res)`.

- `'uniformly` does `'x-uniformly` first and then `'y-uniformly`.


Interactive manipulation
------------------------

`(plot f xlow xhigh 'invisibly . more-adverbs)`

As a reminder, `plot` can be made to do the adaptive point selection,
but show no output.  It returns the plot object, which can be further
manipulated.

`(plot-draw! plot)`

Draw the given plot in a Scheme window and return the plot.  Does not
refine.

`(plot-zoom-x! plot #!optional new-xlow new-xhigh)`

Change the x boundaries of the given plot and compute more points (if
necessary) so that the new view reaches the plot's resolution, the
return the plot.  Useful for looking at a part of the function's
geometry more closely without recomputing the points already plotted,
or at a different region without forgetting the points already
plotted.  If either boundary is left off, set it to include all
avilable already plotted points (and at least the x coordinates -1 and
+1).  If this plot is being followed in a live Scheme window, its
boundaries become the new bounds.

`(plot-zoom-y! plot #!optional new-ylow new-yhigh)`

Clip the plot window to only show the curve within the given
boundaries in the y dimension and compute more points (if necessary)
so that the new view reaches the plot's resolution, then return the
plot.  Useful avoiding being confused by a spike in the function.  If
either boundary is left off, include all y values (including ones the
function may produce in the future).  If this plot is being followed
in a live Scheme window, its boundaries become the new bounds.

`(plot-zoom! plot #!optional new-xlow new-xhigh new-ylow new-yhigh)`

Do both `plot-zoom-x!` and `plot-zoom-y!`.

`(plot-resolve! plot xres yres)`

Change the plot's resolution and compute more points (if necessary) so
that the current view reaches the new resolution, then return the plot.

`(plot-stop-drawing! plot)`

Get rid of the Scheme window (it will not be recreated until you call
`plot-draw!`), and return the plot.

`(plot-gnu! plot . adverbs)`

Draw the (current state of) the plot in a fresh gnuplot window and
return the plot.  Does not compute additional points.  Accepts the
adverbs `'prefixing` and `'commanding`, with the same effect as
`gnuplot`.


Interactive querying
--------------------

`(plot-xlow plot)` `(plot-xhigh plot)` `(plot-ylow plot)` `(plot-yhigh plot)`

Return the appropriate limit for this plot's range of interest.
Returns the default object (which satisfies `default-object?`) if that
boundary is not constrained.

`(plot-dimensions plot)`

Return the bounding box the plot will be drawn in as four values:
`(xlow xhigh ylow yhigh)`.  This is not the same as calling
`plot-xlow`, etc, because it computes what the effective boundaries
are for dimensions that are not constrained.

`(plot-xresolution plot)` `(plot-yresolution plot)`

Return the target x- or y-resolution of the plot.

`(plot-pixels plot)`

Return the plot's target number of pixels.  This is just the product
of the x resolution times the y resolution, but is relevant because
the parabolic refinement algorithm does not care about the aspect
ratio.

`(plot-known-points-alist plot)`

Return all the points the plot has computed so far as a list of x-y
pairs.

`(plot-relevant-points-alist plot)`

Return the points that fall within the plot's current viewport
boundaries (xlow, xhigh, ylow, yhigh) as a list of x-y pairs.

`(plot-count plot)`

Return the number of points the plot has computed so far.

TODO discover the size of the discrepancy
- Maybe this is just a unit testing utility?


Manipulate without autorefinement
---------------------------------

`(new-plot f xlow xhigh)`

Create and return a plot object, but do not make a Scheme window or
start refining.  Does not invoke the function to be plotted.

`(plot-resize-x! plot #!optional xlow xhigh)`,
`(plot-resize-y! plot #!optional ylow yhigh)`,
`(plot-resize!   plot #!optional xlow xhigh ylow yhigh)`

Like `plot-zoom*`, but do not compute additional points until called
for.

`(plot-new-resolution! plot xres yres)`

Like `plot-resolve!`, but do not compute additional points until called
for.

`(plot-refine! plot . adverbs)`

Manually invoke adaptive refinement, according to any `adaptively` or
`uniformly` adverbs supplied.  The default is `'adaptively`.  Return
the plot object.  If there is a live Scheme window following this
plot, it will be updated as refinement proceeds.


Of independent interest
-----------------------

This function is a utility from the perspective of Adaptive Plot, but
may be useful independently of all the rest of this machinery.

`(gnuplot-alist alist ...)`

Plot the given list of points (as x-y pairs) in gnuplot, according to
any supplied `prefixing` and/or `commanding` adverbs.  This is defined
in the file gnuplot.scm, which has no dependencies on any of the rest
of Adaptive Plot, so may be lifted and used elsewhere if desired.

Portability
===========

TODO

Bugs
====

TODO

Author
======

Alexey Radul, axch@mit.edu

License
=======

This file is part of Adaptive Plot, a library for plotting
functions from the MIT Scheme REPL
Copyright (C) 2010-2011 Alexey Radul

Adaptive Plot is free software: you can redistribute it and/or
modify it under the terms of the GNU Affero General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

Adaptive Plot is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with Adaptive Plot.  If not, see
<http://www.gnu.org/licenses/>.
