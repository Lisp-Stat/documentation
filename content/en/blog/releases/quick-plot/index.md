---
title: "Introducing Quick Plot: ggplot-Style Plotting for Lisp-Stat"
date: 2026-02-21
author: "Lisp-Stat Team"
vega: true
description: >
  Composable, ggplot-inspired plotting helpers
tags: ["plotting", "vega-lite", "visualization", "release"]
---

Today we're releasing `quick-plot`, a new system for Lisp-Stat that bring ggplot2-style composable plotting to Common Lisp.  `plot/vega` is able to render any Vega-Lite specification ([here's 50 examples](/docs/cookbooks/plotting/)), but it can at times be a bit wordy constructing the nested plists by hand.

## Overview

Instead of specifying each plot in detail, `quick-plot` provides a set of helper functions that work similarly to `geom` in ggplot. Essentially we borrow ggplot's concept of a plot as a composition of independent layers (which is what Vega-Lite does anyway), each with one concern:

| Function     | Package | Responsibility              |
|--------------|---------|-----------------------------|
| `point`      | `geom`  | Scatter plot mark & encoding|
| `bar`        | `geom`  | Bar chart mark & encoding   |
| `histogram`  | `geom`  | Binned distribution         |
| `box-plot`   | `geom`  | Summary statistics          |
| `line`       | `geom`  | Line chart mark & encoding  |
| `label`      | `gg`    | Axis titles                 |
| `axes`       | `gg`    | Scales, transforms, domains |
| `coord`      | `gg`    | Viewport clipping           |
| `theme`      | `gg`    | Dimensions, fonts, style    |
| `tooltip`    | `gg`    | Hover details               |

Each function returns a plist fragment. A recursive `merge-plists`
combines them into a single Vega-Lite spec. A mark function never
sets axis titles; `label` never touches encodings; `theme` never
alters mark types. You can add, remove, or swap any layer without
affecting the others.

## Simple example

Setup environment:

```lisp
(asdf:load-system :quick-plot)
(vega:load-vega-examples)

(import '(geom:point geom:bar geom:histogram geom:box-plot geom:line))
(import '(gg:label gg:axes gg:coord gg:theme gg:tooltip))
(import '(qplot:qplot))
```

### Basic scatter plot

The simplest scatter plot needs just a name, a data frame, and two
field names:

```lisp
(qplot 'cars-basic vgcars
  `(:title "Horsepower vs. MPG")
   (point :horsepower :miles-per-gallon))
```

{{< vega id="blog-cars-basic" spec="/plots/qplot/cars-basic.vl.json" >}}

### Add labels

Scatter plot with labels:

```lisp
(qplot 'blog-labels vgcars
  `(:title "Vega Cars")
   (point :horsepower :miles-per-gallon :filled t)
   (label :x "Engine Horsepower" :y "Fuel Efficiency (MPG)"))
```

{{< vega id="blog-with-labels" spec="/plots/qplot/cars-with-labels.vl.json" >}}

### Get fancy

Now you can add size of the point to encode some data, along with color:

```lisp
(qplot 'cars-bubble vgcars
  `(:title "Bubble: Size = Acceleration")
   (point :horsepower :miles-per-gallon
          :color :origin :size :acceleration
          :filled t)
   (label :x "Horsepower" :y "Miles per Gallon"))
```

{{< vega id="blog-bubble" spec="/plots/qplot/cars-bubble.vl.json" >}}

## Get started

See the [quick-plot cookbook](/docs/cookbooks/quick-plot/) for recipes for some common plot types.


