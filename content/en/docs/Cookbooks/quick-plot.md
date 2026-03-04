---
title: "Quick Plot"
date: 2026-02-16
weight: 1
vega: true
description: >
  Building plots with composable, ggplot-style helper functions
---

## Overview

The `geom` and `gg` packages provide a set of composable helper
functions for building [Vega-Lite](https://vega.github.io/vega-lite/)
plot specifications. Inspired by R's
[ggplot2](https://ggplot2.tidyverse.org/), specifications are
constructed by combining independent layers — geometry, labels, scales,
coordinates and themes — rather than writing monolithic JSON-like
plists by hand.

Each helper returns a plist. The function `merge-plists` recursively
merges them into a single Vega-Lite spec, which `vega:defplot`
compiles and `plot:plot` renders.

{{% alert title="Package setup" color="info" %}} The examples below
assume you are working in the `LS-USER` package so you need to use the
qualified forms (`geom:point`, `gg:label`, etc.) or import the
appropriate symbols into the current package (which should be
`LS-USER`):

<!-- no-extract -->
```lisp
;; Import marks
(import '(geom:point geom:bar geom:box-plot geom:histogram geom:line))

;; Import modifiers
(import '(gg:label gg:axes gg:coord gg:theme gg:tooltip))

;; Import quick plotting
(import '(qplot:qplot))
```
{{% /alert %}}


### Design philosophy

In ggplot2, a plot is built from independent concerns:

| ggplot2 | Lisp-Stat | Package | Responsibility |
|---------|-----------|---------|----------------|
| `geom_point()` | `point` | `geom` | Mark type and encoding |
| `geom_bar()` | `bar` | `geom` | Mark type and encoding |
| `geom_boxplot()` | `box-plot` | `geom` | Mark type and encoding |
| `geom_histogram()` | `histogram` | `geom` | Binning, aggregation |
| `labs()` | `label` | `gg` | Axis titles |
| `scale_*()` | `axes` | `gg` | Axis transforms, domains, color schemes |
| `coord_cartesian()` | `coord` | `gg` | Viewport clipping |
| `theme()` | `theme` | `gg` | Dimensions, fonts, appearance |

Each function knows about _one_ concern and nothing else. A mark
function never sets axis titles; `label` never touches mark types;
`theme` never alters encodings. This separation means any helper can
be used with any plot type.


### The merge pattern

Every helper returns a plist fragment. `merge-plists` performs a
recursive deep merge: when two plists both supply a nested plist for
the same key (e.g. `:encoding`), the inner plists are merged rather
than one replacing the other. This is what allows `label` to add
`:axis` entries to the `:encoding` that `point` already created.


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
     (point :x-field :y-field)
     (label :x "X Label" :y "Y Label")
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

<!-- no-extract -->
```lisp
(qplot 'my-plot my-data
  `(:title "My Plot")
   (point :x-field :y-field)
   (label :x "X Label" :y "Y Label")
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

<!-- no-extract -->
```lisp
;; First attempt — rough sketch
(qplot 'cars vgcars
  (point :horsepower :miles-per-gallon))

;; Second attempt — add color and a title
(qplot 'cars vgcars
  `(:title "HP vs MPG")
   (point :horsepower :miles-per-gallon :color :origin :filled t))

;; Third attempt — polish for presentation
(qplot 'cars vgcars
  `(:title "HP vs MPG")
   (point :horsepower :miles-per-gallon :color :origin :filled t)
   (label :x "Horsepower" :y "Fuel Efficiency")
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

The layering helpers work with all mark types.

**`label`** — Set axis titles.

```lisp
(label :x "Horsepower" :y "Miles per Gallon")
```

**`axes`** — Set axis types, domains, ranges, and color schemes.

```lisp
(axes :x-type :log :color-scheme :dark2)
(axes :x-domain #(0 300) :y-domain #(0 50))
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

{{% alert title="coord vs. axes" color="info" %}}
`axes` with `:x-domain` changes the axis range but does _not_ clip
marks — points outside the domain overflow visibly.  `coord` sets the
domain _and_ clips marks to the viewport, matching `coord_cartesian()`
semantics.  Choose `coord` when you want to zoom into a region;
choose `axes` when you want to change the axis transform without
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

{{% alert title="Keywords vs. strings for field names" color="info" %}}
Field names are passed as **keywords** (e.g. `:horsepower`,
`:origin`), not strings. The helpers convert keywords to the
camelCase Vega-Lite field names automatically. Only human-readable
labels — axis titles, plot titles, descriptions — are strings.

```lisp
;; Correct — keywords for fields, strings for labels
(point :horsepower :miles-per-gallon :color :origin)
(label :x "Engine Horsepower" :y "Fuel Efficiency")

;; Wrong — strings for field names
(point :horsepower :miles-per-gallon :color "origin")
```
{{% /alert %}}

{{% alert title="Keywords vs. strings for color values" color="warning" %}}
The `:color` parameter accepts both keywords and strings, but they
mean different things:

- **Keyword** → a **data field** mapped to the color encoding channel.
  Each unique value in the field gets a distinct hue.
- **String** → a **literal CSS color** applied directly to the mark.

```lisp
;; Keyword — map the :origin field to color (encoding)
(point :horsepower :miles-per-gallon :color :origin)

;; String — paint every point teal (mark property)
(point :horsepower :miles-per-gallon :color "teal")

;; Same convention applies to all mark types
(histogram :miles-per-gallon :color :origin)        ; stacked by origin
(histogram :miles-per-gallon :color "darkslategray") ; uniform bar color
(bar :origin :miles-per-gallon :color "teal")        ; uniform bar color
```

This convention is consistent across all mark helpers (`point`,
`bar`, `histogram`, `box-plot`, `line`) and all visual channel
parameters (`:color`, `:size`, `:opacity`, etc.): **keywords are
field names, strings are literal values**.
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

### Accessing the helper functions

Let's import our helper functions so we don't need to use package qualified names.
<!-- no-extract -->
```lisp
;; Import marks
(import '(geom:point geom:bar geom:box-plot geom:histogram geom:line))

;; Import modifiers
(import '(gg:label gg:axes gg:coord gg:theme gg:tooltip))

;; Import quick plotting
(import '(qplot:qplot))
```

## Scatter plot

A scatter plot maps two quantitative variables to x and y positions.
Use `point` for exploring relationships, correlations and clusters in
data. The name follows the ggplot2 convention where `geom_point()`
produces a scatter plot.

### Basic scatter plot

The simplest scatter plot needs just a name, a data frame, and two
field names:

```lisp
(qplot 'cars-basic vgcars
  `(:title "Horsepower vs. MPG")
   (point :horsepower :miles-per-gallon))
```

{{< vega id="cars-basic" spec="/plots/qplot/cars-basic.vl.json" >}}

### Color by category

Pass a keyword to `:color` to map a nominal variable to hue. Use
`:filled t` to fill the point marks:

```lisp
(qplot 'cars-colored vgcars
  `(:title "Cars by Origin")
   (point :horsepower :miles-per-gallon
          :color :origin :filled t))
```

{{< vega id="cars-colored" spec="/plots/qplot/cars-colored.vl.json" >}}

### Bubble plot

When `:size` is a keyword, it encodes a third quantitative field as
point area — a bubble plot:

```lisp
(qplot 'cars-bubble vgcars
  `(:title "Bubble: Size = Acceleration")
   (point :horsepower :miles-per-gallon
          :color :origin :size :acceleration
          :filled t)
   (label :x "Horsepower" :y "Miles per Gallon"))
```

{{< vega id="cars-bubble" spec="/plots/qplot/cars-bubble.vl.json" >}}

### Adding axis labels

Use `label` to give axes meaningful titles:

```lisp
(qplot 'cars-with-labels vgcars
  `(:title "Vega Cars")
   (point :horsepower :miles-per-gallon :filled t)
   (label :x "Engine Horsepower" :y "Fuel Efficiency (MPG)"))
```

{{< vega id="cars-with-labels" spec="/plots/qplot/cars-with-labels.vl.json" >}}

### Log scale with custom color scheme

Use `axes` to transform an axis and change the color palette:

```lisp
(qplot 'cars-log-scale vgcars
  `(:title "Horsepower (log) vs. MPG")
   (point :horsepower :miles-per-gallon
          :color :origin :filled t)
   (axes :x-type :log :color-scheme :dark2)
   (label :x "Horsepower (log scale)" :y "Miles per Gallon"))
```

{{< vega id="cars-log-scale" spec="/plots/qplot/cars-log-scale.vl.json" >}}

### Tooltips on hover

Add `tooltip` to show details when the user hovers over a point:

```lisp
(qplot 'cars-tooltip vgcars
  `(:title "Car Details on Hover")
   (point :horsepower :miles-per-gallon
          :color :origin :filled t)
   (tooltip '(:field :name :type :nominal)
            '(:field :horsepower :type :quantitative)
            '(:field :miles-per-gallon :type :quantitative)
            '(:field :origin :type :nominal)))
```

{{< vega id="cars-tooltip" spec="/plots/qplot/cars-tooltip.vl.json" >}}

### Zoom into a region

Use `coord` to restrict the visible area. Unlike `axes`, this
**clips** marks that fall outside the domain — only points within the
viewport are drawn:

```lisp
(qplot 'cars-zoomed vgcars
  `(:title "Cars: 50-150 HP, 20-40 MPG")
   (point :horsepower :miles-per-gallon
          :color :origin :filled t)
   (coord :x-domain #(50 150) :y-domain #(20 40))
   (label :x "Horsepower" :y "Miles per Gallon"))
```

{{< vega id="cars-zoomed" spec="/plots/qplot/cars-zoomed.vl.json" >}}

### Custom theme

Use `theme` to set plot dimensions, font, and visual style:

```lisp
(qplot 'cars-themed vgcars
  `(:title "Themed Scatter Plot")
   (point :horsepower :miles-per-gallon
          :color :origin :filled t)
   (label :x "Horsepower" :y "MPG")
   (theme :width 600 :height 400 :font "Georgia"))
```

{{< vega id="cars-themed" spec="/plots/qplot/cars-themed.vl.json" >}}

### Full example — all layers

Combine every layer for a production-quality plot:

```lisp
(qplot 'cars-full vgcars
  `(:title "Complete Example: All Layers"
    :description "Demonstrating label, axes, tooltip, coord, and theme")
   (point :horsepower :miles-per-gallon
          :color :origin
          :size :acceleration
          :filled t)
   (label :x "Engine Horsepower" :y "Fuel Efficiency (MPG)")
   (axes :color-scheme :category10)
   (tooltip '(:field :name :type :nominal)
            '(:field :horsepower :type :quantitative)
            '(:field :miles-per-gallon :type :quantitative)
            '(:field :origin :type :nominal))
   (coord :x-domain #(40 240) :y-domain #(5 50))
   (theme :width 700 :height 450))
```

{{< vega id="cars-full" spec="/plots/qplot/cars-full.vl.json" >}}

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
   (label :x "Miles per Gallon" :y "Count"))
```

{{< vega id="mpg-hist" spec="/plots/qplot/mpg-hist.vl.json" >}}

### Custom bin count

Control granularity with the `:bin` keyword. Pass a plist with
`:maxbins` to limit the number of bins:

```lisp
(qplot 'mpg-hist-bins vgcars
  `(:title "MPG Distribution (10 bins)")
   (histogram :miles-per-gallon :bin '(:maxbins 10))
   (label :x "Miles per Gallon" :y "Count"))
```

{{< vega id="mpg-hist-bins" spec="/plots/qplot/mpg-hist-bins.vl.json" >}}

### Horizontal histogram

Set `:orient :horizontal` to place bins on the y-axis:

```lisp
(qplot 'mpg-hist-horiz vgcars
  `(:title "MPG Distribution (Horizontal)")
   (histogram :miles-per-gallon :orient :horizontal)
   (label :x "Count" :y "Miles per Gallon"))
```

{{< vega id="mpg-hist-horiz" spec="/plots/qplot/mpg-hist-horiz.vl.json" >}}

### Stacked histogram by group

Pass `:group` to split bins by a nominal field. Vega-Lite
automatically stacks the bars:

```lisp
(qplot 'mpg-hist-stacked vgcars
  `(:title "MPG Distribution by Origin")
   (histogram :miles-per-gallon :group :origin)
   (label :x "Miles per Gallon" :y "Count"))
```

{{< vega id="mpg-hist-stacked" spec="/plots/qplot/mpg-hist-stacked.vl.json" >}}

### Layered histogram

Use `:stack :null` with `:opacity` to overlay distributions
transparently instead of stacking them:

```lisp
(qplot 'mpg-hist-layered vgcars
  `(:title "MPG: Overlaid by Origin")
   (histogram :miles-per-gallon
              :group :origin
              :stack :null
              :opacity 0.5)
   (label :x "Miles per Gallon" :y "Count"))
```

{{< vega id="mpg-hist-layered" spec="/plots/qplot/mpg-hist-layered.vl.json" >}}

### Normalized (100%) stacked histogram

Use `:stack :normalize` to show proportions instead of counts:

```lisp
(qplot 'mpg-hist-normalized vgcars
  `(:title "MPG: Proportion by Origin")
   (histogram :miles-per-gallon
              :group :origin
              :stack :normalize)
   (label :x "Miles per Gallon" :y "Proportion"))
```

{{< vega id="mpg-hist-normalized" spec="/plots/qplot/mpg-hist-normalized.vl.json" >}}

### Styled histogram

Use `:color`, `:corner-radius-end`, and `:bin-spacing` for visual
polish. Combine with `theme` for custom dimensions:

```lisp
(qplot 'mpg-hist-styled vgcars
  `(:title "Styled Histogram")
   (histogram :miles-per-gallon
              :color "darkslategray"
              :corner-radius-end 3
              :bin-spacing 0)
   (label :x "Miles per Gallon" :y "Count")
   (theme :width 500 :height 300))
```

{{< vega id="mpg-hist-styled" spec="/plots/qplot/mpg-hist-styled.vl.json" >}}

## Bar chart

A bar chart maps a categorical variable to position and a
quantitative variable to bar length. Use `bar` when your x-axis is
nominal or ordinal rather than a continuous distribution. The name
follows the ggplot2 convention where `geom_bar()` produces a bar
chart.

### Basic bar chart

Supply the categorical field and the quantitative field:

```lisp
(qplot 'origin-bar vgcars
  `(:title "Average MPG by Origin")
   (bar :origin :miles-per-gallon :aggregate :mean)
   (label :x "Origin" :y "Mean Miles per Gallon"))
```

{{< vega id="origin-bar" spec="/plots/qplot/origin-bar.vl.json" >}}

### Horizontal bar chart

Set `:orient :horizontal` to swap axes — useful for long category
labels:

```lisp
(qplot 'origin-bar-horiz vgcars
  `(:title "Average MPG by Origin (Horizontal)")
   (bar :origin :miles-per-gallon
        :aggregate :mean
        :orient :horizontal)
   (label :x "Mean Miles per Gallon" :y "Origin"))
```

{{< vega id="origin-bar-horiz" spec="/plots/qplot/origin-bar-horiz.vl.json" >}}

### Grouped (stacked) bar chart

Pass `:group` to split bars by a second nominal field. Bars are
stacked by default:

```lisp
(qplot 'cylinders-by-origin vgcars
  `(:title "Car Count: Cylinders by Origin")
   (bar :cylinders :miles-per-gallon
        :aggregate :count
        :group :origin)
   (label :x "Cylinders" :y "Count"))
```

{{< vega id="cylinders-by-origin" spec="/plots/qplot/cylinders-by-origin.vl.json" >}}

### Styled bar chart

Combine visual options with layering helpers:

```lisp
(qplot 'origin-bar-styled vgcars
  `(:title "Mean MPG by Origin")
   (bar :origin :miles-per-gallon
        :aggregate :mean
        :color "teal"
        :corner-radius-end 4)
   (label :x "Origin" :y "Mean MPG")
   (theme :width 400 :height 300 :font "Helvetica"))
```

{{< vega id="origin-bar-styled" spec="/plots/qplot/origin-bar-styled.vl.json" >}}

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
   (label :x "Miles per Gallon"))
```

{{< vega id="mpg-box-1d" spec="/plots/qplot/mpg-box-1d.vl.json" >}}

### 2D box plot — compare groups

Pass `:category` to split the box plot by a nominal field:

```lisp
(qplot 'mpg-box-by-origin vgcars
  `(:title "MPG by Origin")
   (box-plot :miles-per-gallon :category :origin)
   (label :x "Miles per Gallon" :y "Origin"))
```

{{< vega id="mpg-box-by-origin" spec="/plots/qplot/mpg-box-by-origin.vl.json" >}}

### Vertical orientation

Set `:orient :vertical` to place categories on the x-axis and values
on the y-axis:

```lisp
(qplot 'mpg-box-vertical vgcars
  `(:title "MPG by Cylinders (Vertical)")
   (box-plot :miles-per-gallon
             :category :cylinders
             :orient :vertical)
   (label :x "Cylinders" :y "Miles per Gallon"))
```

{{< vega id="mpg-box-vertical" spec="/plots/qplot/mpg-box-vertical.vl.json" >}}

### Min-max whiskers

Set `:extent "min-max"` to extend whiskers to the minimum and
maximum values instead of the default 1.5× IQR (Tukey) whiskers:

```lisp
(qplot 'mpg-box-minmax vgcars
  `(:title "MPG by Origin (Min-Max Whiskers)")
   (box-plot :miles-per-gallon
             :category :origin
             :extent   "min-max") ; note string value
   (label :x "Miles per Gallon" :y "Origin"))
```

{{< vega id="mpg-box-minmax" spec="/plots/qplot/mpg-box-minmax.vl.json" >}}

### Styled box plot

Combine visual options with layering helpers for presentation:

```lisp
(qplot 'mpg-box-styled vgcars
  `(:title "MPG by Origin")
   (box-plot :miles-per-gallon
             :category :origin
             :orient :vertical
             :size 40)
   (label :x "Origin" :y "Miles per Gallon")
   (theme :width 500 :height 350))
```

{{< vega id="mpg-box-styled" spec="/plots/qplot/mpg-box-styled.vl.json" >}}

## Line chart

A line chart connects data points in order, typically along a
temporal or sequential x-axis. Use `line` for time series, trends,
and any data where the relationship between consecutive points is
meaningful. The name follows the ggplot2 convention where
`geom_line()` produces a line chart.

### Basic line chart

The simplest line chart needs two field names. Points are connected
in x-axis order:

```lisp
(qplot 'stock-basic stocks
  `(:title "Google Stock Price"
    :transform #((:filter "datum.symbol === 'GOOG'")))
   (line :date :price :x-type :temporal)
   (label :x "Date" :y "Price (USD)"))
```

{{< vega id="stock-basic" spec="/plots/qplot/stock-basic.vl.json" >}}

### Multiple series by color

Pass a keyword to `:color` to draw a separate line for each
category:

```lisp
(qplot 'stock-colored stocks
  `(:title "Stock Prices by Company")
   (line :date :price :color :symbol :x-type :temporal)
   (label :x "Date" :y "Price (USD)"))
```

{{< vega id="stock-colored" spec="/plots/qplot/stock-colored.vl.json" >}}

### Smoothed interpolation

Set `:interpolate` to control how points are connected. Common
values are `:linear` (default), `:monotone` (smooth, monotonic
curves), `:step` (step function), `:basis` (B-spline), and
`:cardinal`:

```lisp
(qplot 'stock-smooth stocks
  `(:title "Stock Prices (Smoothed)")
   (line :date :price :color :symbol
         :interpolate :monotone
         :x-type :temporal)
   (label :x "Date" :y "Price (USD)"))
```

{{< vega id="stock-smooth" spec="/plots/qplot/stock-smooth.vl.json" >}}

### Line with point markers

Set `:point t` to overlay point marks on each data position — useful
for sparse data or when exact values matter:

```lisp
(qplot 'mpg-trend vgcars
  `(:title "Mean MPG by Model Year")
   (line :year :miles-per-gallon
         :point t :x-type :temporal)
   (label :x "Model Year" :y "Miles per Gallon"))
```

{{< vega id="mpg-trend" spec="/plots/qplot/mpg-trend.vl.json" >}}

### Custom stroke width

Use `:stroke-width` to set a fixed line thickness:

```lisp
(qplot 'stock-thick stocks
  `(:title "AAPL Stock Price"
    :transform #((:filter "datum.symbol === 'AAPL'")))
   (line :date :price
         :stroke-width 3
         :x-type :temporal)
   (label :x "Date" :y "Price (USD)")
   (theme :width 600 :height 300))
```

{{< vega id="stock-thick" spec="/plots/qplot/stock-thick.vl.json" >}}

### Dashed lines

Use `:stroke-dash` with a vector to create dashed or dotted lines.
The vector specifies alternating dash and gap lengths:

```lisp
(qplot 'stock-dashed stocks
  `(:title "Stock Prices (Dashed)")
   (line :date :price
         :color :symbol
         :stroke-dash #(6 3)
         :opacity 0.8
         :x-type :temporal)
   (label :x "Date" :y "Price (USD)"))
```

{{< vega id="stock-dashed" spec="/plots/qplot/stock-dashed.vl.json" >}}

### Step chart

Use `:interpolate :step` for piecewise-constant lines — useful for
data that changes at discrete intervals (e.g. interest rates,
pricing tiers):

```lisp
(qplot 'mpg-step vgcars
  `(:title "MPG by Year (Step)")
   (line :year :miles-per-gallon
         :interpolate :step
         :x-type :temporal)
   (label :x "Model Year" :y "Miles per Gallon"))
```

{{< vega id="mpg-step" spec="/plots/qplot/mpg-step.vl.json" >}}

### Styled multi-series line chart

Combine all options with layering helpers for a polished
presentation:

```lisp
(qplot 'stock-full stocks
  `(:title "Stock Comparison"
    :description "Daily closing prices for major tech stocks")
   (line :date :price
         :color :symbol
         :interpolate :monotone
         :stroke-width 2
         :x-type :temporal)
   (label :x "Date" :y "Closing Price (USD)")
   (axes :color-scheme :dark2)
   (tooltip '(:field :symbol :type :nominal)
            '(:field :date :type :temporal)
            '(:field :price :type :quantitative))
   (theme :width 700 :height 400))
```

{{< vega id="stock-full" spec="/plots/qplot/stock-full.vl.json" >}}

## Combining layers across plot types

Because every helper returns an independent plist, you can mix and
match freely. Here are patterns that work with all mark types.

### Labels on any plot

`label` works identically on every plot type — it only touches
`:encoding :x/:y :axis :title`:

```lisp
;; On a scatter plot
(qplot 'point-labeled vgcars
  (point :horsepower :miles-per-gallon)
  (label :x "Engine Horsepower" :y "Fuel Efficiency"))
```
```lisp
;; On a histogram
(qplot 'hist-labeled vgcars
  (histogram :horsepower)
  (label :x "Engine Horsepower" :y "Number of Cars"))
```
```lisp
;; On a bar chart
(qplot 'bar-labeled vgcars
  (bar :origin :miles-per-gallon :aggregate :mean)
  (label :x "Country of Origin" :y "Average MPG"))
```
```lisp
;; On a box plot
(qplot 'box-labeled vgcars
  (box-plot :miles-per-gallon :category :origin)
  (label :x "Fuel Efficiency" :y "Manufacturing Origin"))
```

### Theme on any plot

`theme` sets top-level properties that apply to every plot type:

```lisp
(qplot 'hist-themed vgcars
  `(:title "Horsepower Distribution")
   (histogram :horsepower :color "darkslategray")
   (label :x "Horsepower" :y "Count")
   (theme :width 500 :height 300 :font "Georgia"))
```

{{< vega id="hist-themed" spec="/plots/qplot/hist-themed.vl.json" >}}

### coord with continuous axes

`coord` works best with quantitative (continuous) axes. For bar
charts and box plots whose axes are categorical, use `coord` with
`:clip nil` to avoid cutting marks, or use `axes` with `:x-domain`
or `:y-domain` instead:

```lisp
;; Zoom a scatter plot — clip marks outside the viewport
(coord :x-domain #(50 150) :y-domain #(20 40))

;; Restrict a bar chart's quantitative axis — no clipping
(coord :y-domain #(0 35) :clip nil)

;; Alternatively, use axes for categorical axes
(axes :y-domain #(0 35))
```

## Recipe: exploring a new dataset

When you first load a dataset, a quick exploratory sequence might
look like this. Note how the same plot name can be reused — each
`qplot` call overwrites the previous definition.

**Step 1 — Distribution of a key variable**

Start with a histogram to understand the shape of the data:

```lisp
(qplot 'explore-1 vgcars
  `(:title "Horsepower Distribution")
   (histogram :horsepower))
```

{{< vega id="explore-1" spec="/plots/qplot/explore-1.vl.json" >}}

**Step 2 — Scatter plot of two main variables**

Look for relationships between variables:

```lisp
(qplot 'explore-2 vgcars
  `(:title "HP vs. MPG")
   (point :horsepower :miles-per-gallon :filled t))
```

{{< vega id="explore-2" spec="/plots/qplot/explore-2.vl.json" >}}

**Step 3 — Compare groups**

Break the data down by category to see if patterns differ:

```lisp
(qplot 'explore-3 vgcars
  `(:title "MPG by Origin")
   (box-plot :miles-per-gallon :category :origin
             :orient :vertical)
   (label :x "Origin" :y "MPG"))
```

{{< vega id="explore-3" spec="/plots/qplot/explore-3.vl.json" >}}

**Step 4 — Add detail**

Color by group and add labels to confirm the story:

```lisp
(qplot 'explore-4 vgcars
  `(:title "HP vs. MPG by Origin")
   (point :horsepower :miles-per-gallon
          :color :origin :filled t)
   (label :x "Horsepower" :y "Miles per Gallon")
   (tooltip '(:field :name :type :nominal)
            '(:field :origin :type :nominal)
            '(:field :horsepower :type :quantitative)
            '(:field :miles-per-gallon :type :quantitative)))
```

{{< vega id="explore-4" spec="/plots/qplot/explore-4.vl.json" >}}

At the REPL, every call overwrites the same `explore` variable.
The variable always holds the latest version:

```lisp
explore             ; => #<VEGA-PLOT ...>
(show-plots)        ; one entry for EXPLORE
```

{{% alert title="Tip" color="info" %}}
For the tutorial we use distinct plot names (`explore-1` through
`explore-4`) so all four plots render on the page. At the REPL you
would reuse `'explore` for all of them — only the latest version
is kept.
{{% /alert %}}


## Recipe: presentation-ready plot

For reports or publications, combine all the layering helpers:

```lisp
(qplot 'presentation vgcars
  `(:title "Automobile Performance by Country of Origin"
    :description "Horsepower vs fuel efficiency for 406 cars")
  (point :horsepower :miles-per-gallon
         :color :origin :filled t)
  (label :x "Engine Horsepower" :y "Fuel Efficiency (MPG)")
  (axes :color-scheme :tableau10)
  (tooltip '(:field :name :type :nominal)
           '(:field :horsepower :type :quantitative)
           '(:field :miles-per-gallon :type :quantitative))
  (theme :width 700 :height 450 :font "Helvetica"))
```

{{< vega id="presentation" spec="/plots/qplot/presentation.vl.json" >}}

## Recipe: saving a plot for later

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
don't support, like transforms, selections, parameters, calculated
fields, multi-view compositions then you can write raw Vega-Lite directly.

**Example: brush selection with linked data table**

This example reproduces the
[Vega-Lite brush table](https://vega.github.io/vega-lite/examples/brush_table.html):
drag a rectangle over the scatter plot to see the selected cars in a
table. It uses `hconcat` (side-by-side views), `params` (interactive
brush), and `transform` (filter + rank) — none of which the helpers
generate. For specs this complex, write the full Vega-Lite plist and
use `defplot` directly:

```lisp
(plot:plot
 (vega:defplot brush-table
   `(:description "Drag a rectangular brush to show selected points in a table."
     :data (:values ,vgcars)
     :transform #((:window #((:op :row-number :as "row_number"))))
     :hconcat
     #(;; Left panel: scatter plot with brush
       (:params #((:name "brush" :select "interval"))
        :mark :point
        :encoding
        (:x (:field :horsepower :type :quantitative)
         :y (:field :miles-per-gallon :type :quantitative)
         :color (:condition (:param "brush"
                             :field :cylinders
                             :type :ordinal)
                 :value "grey")))

       ;; Right panel: table of selected points
       (:transform #((:filter (:param "brush"))
                     (:window #((:op :rank :as "rank")))
                     (:filter (:field "rank" :lt 20)))
        :hconcat
        #((:width 50
           :title "Horsepower"
           :mark :text
           :encoding
           (:text (:field :horsepower :type :nominal)
            :y (:field "row_number" :type :ordinal :axis :null)))
          (:width 50
           :title "MPG"
           :mark :text
           :encoding
           (:text (:field :miles-per-gallon :type :nominal)
            :y (:field "row_number" :type :ordinal :axis :null)))
          (:width 50
           :title "Origin"
           :mark :text
           :encoding
           (:text (:field :origin :type :nominal)
            :y (:field "row_number" :type :ordinal :axis :null))))))
     :resolve (:legend (:color :independent)))))
```

{{< vega id="brush-table" spec="/plots/qplot/brush-table.vl.json" >}}

This plot cannot be built with `qplot` and the layering helpers;
the `hconcat` layout requires two independent views with different
marks, encodings, and transforms. The rule of thumb:

- **Use `qplot` + helpers** for single-view plots (scatter, line,
  bar, histogram, box plot) with standard encodings
- **Use `defplot` + raw plists** for multi-view layouts (`hconcat`,
  `vconcat`, `layer`, `facet`), complex interactions, or any
  Vega-Lite feature the helpers don't cover

Both approaches produce the same `vega-plot` objects and coexist
in the same session.
