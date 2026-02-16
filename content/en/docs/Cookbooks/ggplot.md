---
title: "ggplot"
date: 2026-02-15
weight: 10
description: >
  Building plots with composable, ggplot-style helper functions
---

## Overview

The `geom` package provides a set of composable helper functions for
building [Vega-Lite](https://vega.github.io/vega-lite/) plot
specifications. Inspired by R's
[ggplot2](https://ggplot2.tidyverse.org/), specifications are
constructed by combining independent layers — geometry, labels, scales,
coordinates and themes — rather than writing monolithic JSON-like
plists by hand.

Each helper returns a plain plist. The function `merge-plists`
recursively merges them into a single Vega-Lite spec, which
`vega:defplot` compiles and `plot:plot` renders.


### Design philosophy

In ggplot2, a plot is built from independent concerns:

| ggplot2 | Lisp-Stat | Responsibility |
|---------|-----------|----------------|
| `geom_point()` | `scatter-plot` | Mark type and encoding |
| `geom_bar()` | `bar-chart` | Mark type and encoding |
| `geom_boxplot()` | `box-plot` | Mark type and encoding |
| `geom_histogram()` | `histogram` | Binning, aggregation |
| `labs()` | `labs` | Axis titles |
| `scale_*()` | `scale` | Axis transforms, domains, color schemes |
| `coord_cartesian()` | `coord` | Viewport clipping |
| `theme()` | `theme` | Dimensions, fonts, appearance |

Each function knows about _one_ concern and nothing else. A `geom`
function never sets axis titles; `labs` never touches mark types;
`theme` never alters encodings. This separation means any helper can
be used with any plot type.


### The merge pattern

Every helper returns a plist fragment. `merge-plists` performs a
recursive deep merge: when two plists both supply a nested plist for
the same key (e.g. `:encoding`), the inner plists are merged rather
than one replacing the other. This is what allows `labs` to add
`:axis` entries to the `:encoding` that `scatter-plot` already created.


### Two ways to plot

Lisp-Stat provides two entry points for creating plots. Choose the one
that fits your workflow.

#### `defplot` + `plot:plot` — the explicit pattern

The traditional approach separates definition from rendering. Use this
when you want full control, or when writing scripts and notebooks:

```lisp
(plot:plot
 (vega:defplot my-plot
   (merge-plists
    `(:title "My Plot"
      :data (:values ,my-data))
    (scatter-plot :x-field :y-field)
    (labs :x "X Label" :y "Y Label")
    (theme :width 600))))
```

`defplot` is a macro that:

1. Calls `%defplot` to create a `vega-plot` object
2. Binds it to a global variable (`my-plot`)
3. Registers it in `*all-plots*` so `show-plots` can list it

`plot:plot` then renders the object to the browser.

#### `qplot` — quick plot for the REPL

For interactive exploration, the three-line scaffold of `plot:plot` /
`vega:defplot` / `merge-plists` is repetitive. `qplot` collapses it
into a single function call:

```lisp
(qplot 'my-plot my-data
  `(:title "My Plot")
  (scatter-plot :x-field :y-field)
  (labs :x "X Label" :y "Y Label")
  (theme :width 600))
```

`qplot` takes a **name** (a symbol), a **data** object (a data frame,
plist data, or URI), and any number of **layer** plists. It:

1. Prepends `(:data (:values ,data))` and merges all layers
2. Creates the plot object via `%defplot`
3. Binds it to the named global variable
4. Registers it in `*all-plots*`
5. Renders immediately via `plot:plot`
6. Returns the plot object

Because `qplot` binds a named variable, the standard REPL workflow is
to **re-evaluate the same form** as you iterate on a plot. Each call
overwrites the previous definition — there is no accumulation in
`*all-plots*` or in the global namespace:

```lisp
;; First attempt — rough sketch
(qplot 'cars vgcars
  (scatter-plot :horsepower :miles-per-gallon))

;; Second attempt — add color and a title
(qplot 'cars vgcars
  `(:title "HP vs MPG")
  (scatter-plot :horsepower :miles-per-gallon :color "origin" :filled t))

;; Third attempt — polish for presentation
(qplot 'cars vgcars
  `(:title "HP vs MPG")
  (scatter-plot :horsepower :miles-per-gallon :color "origin" :filled t)
  (labs :x "Horsepower" :y "Fuel Efficiency")
  (theme :width 600 :height 400))

;; Later — the variable is still bound
cars                ; => #<VEGA-PLOT ...>
(plot:plot cars)    ; re-render
(describe cars)     ; inspect the spec
(show-plots)        ; lists one 'cars' entry
```

{{% alert title="qplot vs defplot" color="info" %}}
`qplot` and `defplot` produce identical plot objects. The only
differences are syntactic: `qplot` separates the data argument,
handles the merge, and renders immediately. You can freely mix both
styles in the same session — they share `*all-plots*` and the global
namespace.
{{% /alert %}}


### Helpers reference

The layering helpers work with all geom types.

**`labs`** — Set axis titles.

```lisp
(labs :x "Horsepower" :y "Miles per Gallon")
```

**`scale`** — Set axis types, domains, ranges, and color schemes.

```lisp
(scale :x-type :log :color-scheme :dark2)
(scale :x-domain #(0 300) :y-domain #(0 50))
```

**`tooltip`** — Add hover tooltips. Each argument is a field spec.

```lisp
(tooltip '(:field :name :type :nominal)
         '(:field :horsepower :type :quantitative))
```

**`coord`** — Restrict the visible viewport and clip marks, like
`coord_cartesian()` in ggplot2. Only data within the domain is
visible; points outside are clipped rather than dropped.

```lisp
(coord :x-domain #(50 150) :y-domain #(20 40))
```

**`theme`** — Set dimensions, font, background, or named presets.

```lisp
(theme :width 600 :height 400 :font "Georgia")
```

{{% alert title="coord vs. scale" color="info" %}}
`scale` with `:x-domain` changes the axis range but does _not_ clip
marks — points outside the domain overflow visibly.  `coord` sets the
domain _and_ clips marks to the viewport, matching `coord_cartesian()`
semantics.  Choose `coord` when you want to zoom into a region;
choose `scale` when you want to change the axis transform without
hiding data.
{{% /alert %}}

{{% alert title="Vectors for JSON arrays" color="warning" %}}
Vega-Lite expects JSON arrays for `domain`, `range`, `tooltip`, and
similar properties.  In the JSON serializer, **lists** become JSON
objects and **vectors** become JSON arrays.  Always use `#(...)` or
`(vector ...)` when you need a JSON array:

```lisp
;; Correct — produces [50, 150]
(coord :x-domain #(50 150))

;; Wrong — produces {"50": 150}
(coord :x-domain '(50 150))
```
{{% /alert %}}


### Loading example data

The examples below use datasets from the [Vega datasets](https://github.com/vega/vega-datasets) collection.
Load them into your session before running the examples:

```lisp
(vega:load-vega-examples)
```

This makes variables like `vgcars` (a data frame of automobile
specifications) available in your environment.  For datasets loaded
using `vega:read-vega`, field names are automatically converted to
Lisp-style keywords (e.g. `Miles_per_Gallon` becomes
`:miles-per-gallon`).


## Scatter plot

A scatter plot maps two quantitative variables to x and y positions.
Use `scatter-plot` for exploring relationships, correlations and
clusters in data.

### Basic scatter plot

The simplest scatter plot needs just a name, a data frame, and two
field names:

```lisp
(qplot 'cars-basic vgcars
  `(:title "Horsepower vs. MPG")
  (scatter-plot :horsepower :miles-per-gallon))
```

### Color by category

Pass a string field name to `:color` to map a nominal variable to
hue. Use `:filled t` to fill the point marks:

```lisp
(qplot 'cars-colored vgcars
  `(:title "Cars by Origin")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :filled t))
```

### Bubble plot

When `:size` is a string, it encodes a third quantitative field as
point area — a bubble plot:

```lisp
(qplot 'cars-bubble vgcars
  `(:title "Bubble: Size = Acceleration")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :size "acceleration"
                :filled t)
  (labs :x "Horsepower" :y "Miles per Gallon"))
```

### Adding axis labels

Use `labs` to give axes meaningful titles:

```lisp
(qplot 'cars-with-labels vgcars
  `(:title "Vega Cars")
  (scatter-plot :horsepower :miles-per-gallon :filled t)
  (labs :x "Engine Horsepower" :y "Fuel Efficiency (MPG)"))
```

### Log scale with custom color scheme

Use `scale` to transform an axis and change the color palette:

```lisp
(qplot 'cars-log-scale vgcars
  `(:title "Horsepower (log) vs. MPG")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :filled t)
  (scale :x-type :log :color-scheme :dark2)
  (labs :x "Horsepower (log scale)" :y "Miles per Gallon"))
```

### Tooltips on hover

Add `tooltip` to show details when the user hovers over a point:

```lisp
(qplot 'cars-tooltip vgcars
  `(:title "Car Details on Hover")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :filled t)
  (tooltip '(:field :name :type :nominal)
           '(:field :horsepower :type :quantitative)
           '(:field :miles-per-gallon :type :quantitative)
           '(:field :origin :type :nominal)))
```

### Zoom into a region

Use `coord` to restrict the visible area. Unlike `scale`, this
**clips** marks that fall outside the domain — only points within the
viewport are drawn:

```lisp
(qplot 'cars-zoomed vgcars
  `(:title "Cars: 50-150 HP, 20-40 MPG")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :filled t)
  (coord :x-domain #(50 150) :y-domain #(20 40))
  (labs :x "Horsepower" :y "Miles per Gallon"))
```

### Custom theme

Use `theme` to set plot dimensions, font, and visual style:

```lisp
(qplot 'cars-themed vgcars
  `(:title "Themed Scatter Plot")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :filled t)
  (labs :x "Horsepower" :y "MPG")
  (theme :width 600 :height 400 :font "Georgia"))
```

### Full example — all layers

Combine every layer for a production-quality plot:

```lisp
(qplot 'cars-full vgcars
  `(:title "Complete Example: All Layers"
    :description "Demonstrating labs, scale, tooltip, coord, and theme")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin"
                :size "acceleration"
                :filled t)
  (labs :x "Engine Horsepower" :y "Fuel Efficiency (MPG)")
  (scale :color-scheme :category10)
  (tooltip '(:field :name :type :nominal)
           '(:field :horsepower :type :quantitative)
           '(:field :miles-per-gallon :type :quantitative)
           '(:field :origin :type :nominal))
  (coord :x-domain #(40 240) :y-domain #(5 50))
  (theme :width 700 :height 450))
```


## Histogram

A histogram bins a quantitative variable and counts observations per
bin. Use `histogram` to visualize the distribution of a single
variable.

### Basic histogram

Pass a single field name. The default uses Vega-Lite's automatic
binning and counts occurrences:

```lisp
(qplot 'mpg-hist vgcars
  `(:title "Distribution of Miles per Gallon")
  (histogram :miles-per-gallon)
  (labs :x "Miles per Gallon" :y "Count"))
```

### Custom bin count

Control granularity with the `:bin` keyword. Pass a plist with
`:maxbins` to limit the number of bins:

```lisp
(qplot 'mpg-hist-bins vgcars
  `(:title "MPG Distribution (10 bins)")
  (histogram :miles-per-gallon :bin '(:maxbins 10))
  (labs :x "Miles per Gallon" :y "Count"))
```

### Horizontal histogram

Set `:orient :horizontal` to place bins on the y-axis:

```lisp
(qplot 'mpg-hist-horiz vgcars
  `(:title "MPG Distribution (Horizontal)")
  (histogram :miles-per-gallon :orient :horizontal)
  (labs :x "Count" :y "Miles per Gallon"))
```

### Stacked histogram by group

Pass `:group` to split bins by a nominal field. Vega-Lite
automatically stacks the bars:

```lisp
(qplot 'mpg-hist-stacked vgcars
  `(:title "MPG Distribution by Origin")
  (histogram :miles-per-gallon :group "origin")
  (labs :x "Miles per Gallon" :y "Count"))
```

### Layered histogram

Use `:stack :null` with `:opacity` to overlay distributions
transparently instead of stacking them:

```lisp
(qplot 'mpg-hist-layered vgcars
  `(:title "MPG: Overlaid by Origin")
  (histogram :miles-per-gallon
             :group "origin"
             :stack :null
             :opacity 0.5)
  (labs :x "Miles per Gallon" :y "Count"))
```

### Normalized (100%) stacked histogram

Use `:stack :normalize` to show proportions instead of counts:

```lisp
(qplot 'mpg-hist-normalized vgcars
  `(:title "MPG: Proportion by Origin")
  (histogram :miles-per-gallon
             :group "origin"
             :stack :normalize)
  (labs :x "Miles per Gallon" :y "Proportion"))
```

### Styled histogram

Use `:color`, `:corner-radius-end`, and `:bin-spacing` for visual
polish. Combine with `theme` for custom dimensions:

```lisp
(qplot 'mpg-hist-styled vgcars
  `(:title "Styled Histogram")
  (histogram :miles-per-gallon
             :color :darkslategray
             :corner-radius-end 3
             :bin-spacing 0)
  (labs :x "Miles per Gallon" :y "Count")
  (theme :width 500 :height 300))
```


## Bar chart

A bar chart maps a categorical variable to position and a
quantitative variable to bar length. Use `bar-chart` when your x-axis
is nominal or ordinal rather than a continuous distribution.

### Basic bar chart

Supply the categorical field and the quantitative field:

```lisp
(qplot 'origin-bar vgcars
  `(:title "Average MPG by Origin")
  (bar-chart "origin" :miles-per-gallon :aggregate :mean)
  (labs :x "Origin" :y "Mean Miles per Gallon"))
```

### Horizontal bar chart

Set `:orient :horizontal` to swap axes — useful for long category
labels:

```lisp
(qplot 'origin-bar-horiz vgcars
  `(:title "Average MPG by Origin (Horizontal)")
  (bar-chart "origin" :miles-per-gallon
             :aggregate :mean
             :orient :horizontal)
  (labs :x "Mean Miles per Gallon" :y "Origin"))
```

### Grouped (stacked) bar chart

Pass `:group` to split bars by a second nominal field. Bars are
stacked by default:

```lisp
(qplot 'cylinders-by-origin vgcars
  `(:title "Car Count: Cylinders by Origin")
  (bar-chart "cylinders" :miles-per-gallon
             :aggregate :count
             :group "origin")
  (labs :x "Cylinders" :y "Count"))
```

### Styled bar chart

Combine visual options with layering helpers:

```lisp
(qplot 'origin-bar-styled vgcars
  `(:title "Mean MPG by Origin")
  (bar-chart "origin" :miles-per-gallon
             :aggregate :mean
             :color :teal
             :corner-radius-end 4)
  (labs :x "Origin" :y "Mean MPG")
  (theme :width 400 :height 300 :font "Helvetica"))
```


## Box plot

A box plot summarizes the distribution of a quantitative variable,
showing the median, interquartile range and outliers. Use `box-plot`
to compare distributions across groups.

### 1D box plot

A single quantitative field produces a box plot summarizing the
entire variable:

```lisp
(qplot 'mpg-box-1d vgcars
  `(:title "MPG Distribution")
  (box-plot :miles-per-gallon)
  (labs :x "Miles per Gallon"))
```

### 2D box plot — compare groups

Pass `:category` to split the box plot by a nominal field:

```lisp
(qplot 'mpg-box-by-origin vgcars
  `(:title "MPG by Origin")
  (box-plot :miles-per-gallon :category "origin")
  (labs :x "Miles per Gallon" :y "Origin"))
```

### Vertical orientation

Set `:orient :vertical` to place categories on the x-axis and values
on the y-axis:

```lisp
(qplot 'mpg-box-vertical vgcars
  `(:title "MPG by Cylinders (Vertical)")
  (box-plot :miles-per-gallon
            :category "cylinders"
            :orient :vertical)
  (labs :x "Cylinders" :y "Miles per Gallon"))
```

### Min-max whiskers

Set `:extent "min-max"` to extend whiskers to the minimum and
maximum values instead of the default 1.5× IQR (Tukey) whiskers:

```lisp
(qplot 'mpg-box-minmax vgcars
  `(:title "MPG by Origin (Min-Max Whiskers)")
  (box-plot :miles-per-gallon
            :category "origin"
            :extent "min-max")
  (labs :x "Miles per Gallon" :y "Origin"))
```

### Styled box plot

Combine visual options with layering helpers for presentation:

```lisp
(qplot 'mpg-box-styled vgcars
  `(:title "MPG by Origin")
  (box-plot :miles-per-gallon
            :category "origin"
            :orient :vertical
            :size 40)
  (labs :x "Origin" :y "Miles per Gallon")
  (theme :width 500 :height 350))
```


## Combining layers across plot types

Because every helper returns an independent plist, you can mix and
match freely. Here are patterns that work with all geom types.

### Labels on any plot

`labs` works identically on every plot type — it only touches
`:encoding :x/:y :axis :title`:

```lisp
;; On a histogram
(qplot 'hist-labeled vgcars
  (histogram :horsepower)
  (labs :x "Engine Horsepower" :y "Number of Cars"))

;; On a bar chart
(qplot 'bar-labeled vgcars
  (bar-chart "origin" :miles-per-gallon :aggregate :mean)
  (labs :x "Country of Origin" :y "Average MPG"))

;; On a box plot
(qplot 'box-labeled vgcars
  (box-plot :miles-per-gallon :category "origin")
  (labs :x "Fuel Efficiency" :y "Manufacturing Origin"))
```

### Theme on any plot

`theme` sets top-level properties that apply to every plot type:

```lisp
(qplot 'hist-themed vgcars
  `(:title "Horsepower Distribution")
  (histogram :horsepower :color :darkslategray)
  (labs :x "Horsepower" :y "Count")
  (theme :width 500 :height 300 :font "Georgia"))
```

### coord with continuous axes

`coord` works best with quantitative (continuous) axes. For bar
charts and box plots whose axes are categorical, use `coord` with
`:clip nil` to avoid cutting marks, or use `scale` with `:x-domain`
or `:y-domain` instead:

```lisp
;; Zoom a scatter plot — clip marks outside the viewport
(coord :x-domain #(50 150) :y-domain #(20 40))

;; Restrict a bar chart's quantitative axis — no clipping
(coord :y-domain #(0 35) :clip nil)

;; Alternatively, use scale for categorical axes
(scale :y-domain #(0 35))
```


## Recipes

### Recipe: exploring a new dataset

When you first load a dataset, a quick exploratory sequence might
look like this. Note how the same plot name can be reused — each
`qplot` call overwrites the previous definition:

```lisp
;; 1. Distribution of a key variable
(qplot 'explore vgcars
  `(:title "Horsepower Distribution")
  (histogram :horsepower))

;; 2. Refine — scatter plot of two main variables
(qplot 'explore vgcars
  `(:title "HP vs. MPG")
  (scatter-plot :horsepower :miles-per-gallon :filled t))

;; 3. Refine — compare groups
(qplot 'explore vgcars
  `(:title "MPG by Origin")
  (box-plot :miles-per-gallon :category "origin"
            :orient :vertical)
  (labs :x "Origin" :y "MPG"))

;; The variable 'explore' always holds the latest version
explore             ; => #<VEGA-PLOT ...>
(show-plots)        ; one entry for EXPLORE
```

### Recipe: presentation-ready plot

For reports or publications, combine all the layering helpers:

```lisp
(qplot 'presentation vgcars
  `(:title "Automobile Performance by Country of Origin"
    :description "Horsepower vs fuel efficiency for 406 cars")
  (scatter-plot :horsepower :miles-per-gallon
                :color "origin" :filled t)
  (labs :x "Engine Horsepower" :y "Fuel Efficiency (MPG)")
  (scale :color-scheme :tableau10)
  (tooltip '(:field :name :type :nominal)
           '(:field :horsepower :type :quantitative)
           '(:field :miles-per-gallon :type :quantitative))
  (theme :width 700 :height 450 :font "Helvetica"))
```

### Recipe: saving a plot for later

Once you are happy with a plot created via `qplot`, it is a
first-class `vega-plot` object. You can re-render, inspect, or
write it to a file:

```lisp
;; Re-render in the browser
(plot:plot presentation)

;; Inspect the spec
(describe presentation)

;; List all named plots
(show-plots)
```

### Recipe: dropping down to raw Vega-Lite

The helpers cover common patterns. When you need something they
don't support — transforms, selections, parameters, calculated
fields, layered compositions — write the raw Vega-Lite plist
directly and pass it as a layer alongside the helpers. `qplot`
merges everything through `merge-plists`, so helpers and hand-written
plists interoperate without friction.

**Example: adding a Vega-Lite transform and interactive selection**

Neither `scatter-plot` nor any helper generates `:transform` or
`:params` — these are raw Vega-Lite keys written by hand:

```lisp
(qplot 'filtered-interactive vgcars
  `(:title "European Cars (HP > 50) — Brush to Select"

    ;; Raw Vega-Lite: filter transform — no helper for this
    :transform #((:filter "datum.origin === 'Europe' && datum.horsepower > 50"))

    ;; Raw Vega-Lite: interactive brush selection — no helper for this
    :params #((:name "brush"
               :select (:type :interval))))

  ;; Helpers for the parts they handle well
  (scatter-plot :horsepower :miles-per-gallon :filled t)
  (labs :x "Horsepower" :y "MPG")
  (theme :width 600 :height 400))
```

In this example, three layers are merged:

1. **A hand-written plist** with `:title`, `:transform`, and `:params` —
   raw Vega-Lite that no helper generates
2. **`scatter-plot`** — the mark and encoding
3. **`labs`** and **`theme`** — axis titles and dimensions

`merge-plists` combines them into a single spec. The rule is simple:
use helpers when they exist, write raw plists when they don't. You
never have to choose one approach over the other.