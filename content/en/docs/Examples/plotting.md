---
title: "Plotting"
date: 2021-04-26
weight: 3
description: >
  Example plots
---

The plots here show equivalents to the [Vega-Lite example
gallery](https://vega.github.io/vega-lite/examples/). 


## Preliminaries

### Load Vega-Lite

Load Vega-Lite and network libraries:

```lisp
(ql:quickload :lisp-stat)
(ql:quickload :plot/vglt)
(ql:quickload :dexador)
(ql:quickload :access)
```

### Load example data

```lisp
(in-package :lisp-stat)
(defparameter vega-cars
  (vglt:vl-to-df
    (dex:get
	  "https://raw.githubusercontent.com/vega/vega-datasets/master/data/cars.json"
	  :want-stream t)))
```

## Strip plot
The Vega-Lite [strip plot example
shows](https://vega.github.io/vega-lite/examples/tick_strip.html) the
relationship between horsepower and the number of cylinders using tick
marks.

{{< figure src="/docs/examples/vega-cars-strip-plot.png" >}}

In this example we will show how to build a spec from beginning to
end, without using a plot template.

JSON
```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "Shows the relationship between horsepower and the number of cylinders using tick marks.",
  "data": {"url": "data/cars.json"},
  "mark": "tick",
  "encoding": {
    "x": {"field": "Horsepower", "type": "quantitative"},
    "y": {"field": "Cylinders", "type": "ordinal"}
  }
}
```

Lisp-Stat
```lisp
(defparameter cars-strip-plot
  (line-up-first
	(vglt:spec)
    (vglt:add "description" "Shows the relationship between horsepower and the number of cylinders using tick marks.")
	(vglt:add "data" `(("values" . ,(vglt:df-to-alist vega-cars))))
	(vglt:add "mark" "tick")
	(vglt:add "encoding" '(("x" ("field" . "HORSEPOWER") ("type" . "quantitative") ("title" . "Horsepower"))
	                       ("y" ("field" . "CYLINDERS")  ("type" . "ordinal") ("title" . "Cylinders"))))))
(plot:plot-from-file (vglt:save-plot 'cars-strip-plot))
```

## Scatter plots

### Basic
A [basic Vega-Lite
scatterplot](https://vega.github.io/vega-lite/examples/point_2d.html)
showing horsepower and miles per gallons for various cars.

{{< figure src="/docs/examples/vega-cars-scatter-plot.png" title="Horsepower vs. MPG scatter plot" >}}

In this example we use the Lisp-Stat template for a basic scatter plot.

JSON
```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "A scatterplot showing horsepower and miles per gallons for various cars.",
  "data": {"url": "data/cars.json"},
  "mark": "point",
  "encoding": {
    "x": {"field": "Horsepower", "type": "quantitative"},
    "y": {"field": "Miles_per_Gallon", "type": "quantitative"}
  }
}
```

Lisp-Stat
```lisp
(defparameter cars-scatter-plot
  (vglt:scatter-plot vega-cars "HORSEPOWER" "MILES_PER_GALLON"))
(plot:plot-from-file (vglt:save-plot 'cars-scatter-plot))
```

### Colored

{{< figure src="/docs/examples/vega-cars-colored-scatter-plot.png" >}}

In this example we'll show how to modify a plot that was based on one
of the the Lisp-Stat plotting templates. We'd like to add some
additional information to the cars scatter plot to show the cars
origin. The [Vega-Lite
example](https://vega.github.io/vega-lite/examples/point_color_with_shape.html)
shows that we have to add two new directives to the _encoding_ of the
plot:

```lisp
(pushnew
 '("color" . (("field" . "ORIGIN") ("type" . "nominal")))
 (access:accesses cars-scatter-plot :encoding))
(pushnew
 '("shape" . (("field" . "ORIGIN") ("type" . "nominal")))
 (access:accesses cars-scatter-plot :encoding))
(plot:plot-from-file (vglt:save-plot 'cars-scatter-plot))
```

With this change we can see that the higher horsepower, lower
efficiency, cars are from the USA, and the higher efficiency cars from
Japan and Europe.

### Text marks

The same information, but further indicated with a text marker. This
[Vega-Lite
example](https://vega.github.io/vega-lite/examples/text_scatterplot_colored.html)
is sufficiently different from the template that we'll construct it
all here. Notice the use of a data transformation.

{{< figure src="/docs/examples/vega-cars-text-scatter-plot.png" >}}

JSON
```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "data": {"url": "data/cars.json"},
  "transform": [{
    "calculate": "datum.Origin[0]",
    "as": "OriginInitial"
  }],
  "mark": "text",
  "encoding": {
    "x": {"field": "Horsepower", "type": "quantitative"},
    "y": {"field": "Miles_per_Gallon", "type": "quantitative"},
    "color": {"field": "Origin", "type": "nominal"},
    "text": {"field": "OriginInitial", "type": "nominal"}
  }
}
```
Lisp-Stat
```lisp
(defparameter cars-scatter-text-plot
   (line-up-first
    (vglt:spec)
	(vglt:add "data" `(("values" . ,(vglt:df-to-alist vega-cars))))
	(vglt:add "transform" #((("calculate" . "datum.ORIGIN[0]") ("as" . "OriginInitial"))))
	(vglt:add "mark" "text")
	(vglt:add "encoding" '(("x" ("field" . "HORSEPOWER") ("type" . "quantitative") ("title" . "Horsepower"))
	                       ("y" ("field" . "MILES_PER_GALLON") ("type" . "quantitative") ("title" . "Miles per Gallon"))
	                       ("color" . (("field" . "ORIGIN") ("type" . "nominal")))
					       ("text" . (("field" . "OriginInitial") ("type" . "nominal")))))))
(plot:plot-from-file (vglt:save-plot 'cars-scatter-text-plot))
```

<!--
This example is broken on the Vega-Lite website. Even copy & paste into a HTML file does not work.
### Mean & SD overlay

This Vega-Lite [scatterplot with mean and standard deviation
overlay](https://vega.github.io/vega-lite/examples/layer_scatter_errorband_1D_stdev_global_mean.html)
demonstrates the use of layers in a plot.

{{< figure src="/docs/examples/vega-cars-mean-sd.png" >}}

JSON

```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "A scatterplot showing horsepower and miles per gallons for various cars.",
  "data": {"url": "data/cars.json"},
  "layer": [
    {
      "mark": "point",
      "encoding": {
        "x": {"field": "Horsepower", "type": "quantitative"},
        "y": {"field": "Miles_per_Gallon", "type": "quantitative"}
      }
    },
    {
      "mark": {"type": "errorband", "extent": "stdev", "opacity": 0.2},
      "encoding": {
        "y": {
          "field": "Miles_per_Gallon",
          "type": "quantitative",
          "title": "Miles per Gallon"
        }
      }
    },
    {
      "mark": "rule",
      "encoding": {
        "y": {
          "field": "Miles_per_Gallon",
          "type": "quantitative",
          "aggregate": "mean"
        }
      }
    }
  ]
}
```

Lisp-Stat equivalent

```lisp

```
-->

### Interactive scatter plot matrix

This [Vega-Lite interactive scatter plot
matrix](https://vega.github.io/vega-lite/examples/interactive_splom.html)
includes interactive elements and demonstrates creating a SPLOM
(scatter plot matrix).

{{< figure src="/docs/examples/vega-cars-interactive-scatter-plot.png" >}}

Above is a PNG file. The interactive version is <a href="/cars-interactive-splom.html" target="_blank">here</a>.


JSON
```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "repeat": {
    "row": ["Horsepower", "Acceleration", "Miles_per_Gallon"],
    "column": ["Miles_per_Gallon", "Acceleration", "Horsepower"]
  },
  "spec": {
    "data": {"url": "data/cars.json"},
    "mark": "point",
    "params": [
      {
        "name": "brush",
        "select": {
          "type": "interval",
          "resolve": "union",
          "on": "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!",
          "translate": "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!",
          "zoom": "wheel![event.shiftKey]"
        }
      },
      {
        "name": "grid",
        "select": {
          "type": "interval",
          "resolve": "global",
          "translate": "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!",
          "zoom": "wheel![!event.shiftKey]"
        },
        "bind": "scales"
      }
    ],
    "encoding": {
      "x": {"field": {"repeat": "column"}, "type": "quantitative"},
      "y": {
        "field": {"repeat": "row"},
        "type": "quantitative",
        "axis": {"minExtent": 30}
      },
      "color": {
        "condition": {
          "param": "brush",
          "field": "Origin",
          "type": "nominal"
        },
        "value": "grey"
      }
    }
  }
}
```

Lisp-Stat equivalent
```lisp
(defparameter cars-interactive-splom
  (line-up-first
   (vglt:spec)
   (vglt:add "repeat" '(("row" . #("HORSEPOWER" "ACCELERATION" "MILES_PER_GALLON"))
			            ("column" . #("MILES_PER_GALLON" "ACCELERATION" "HORSEPOWER"))))
   (vglt:add "spec"
             `(("data" ("values" . ,(vglt:df-to-alist vega-cars)))
		      ("mark" . "point")
		      ("params" . #(
			        (("name" . "brush")
				     ("select"
				      ("type" . "interval")
				      ("resolve" . "union")
				      ("on" . "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
				      ("translate" . "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
				      ("zoom" . "wheel![event.shiftKey]")))
				    (("name" . "grid")
				     ("select"
				      ("type" . "interval")
				      ("resolve" . "global")
				      ("translate" . "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!")
				      ("zoom" . "wheel![!event.shiftKey]"))
				      ("bind" . "scales"))))
		      ("encoding" . (("x" ("field" ("repeat" . "column")) ("type" . "quantitative"))
				             ("y" ("field" ("repeat" . "row")) ("type" . "quantitative") ("axis" ("minExtent" . 30)))
				             ("color" ("condition" ("param" . "brush")
							                       ("field" . "ORIGIN")
							                       ("type" . "nominal"))
					                  ("value" . "grey"))))))))
(plot:plot-from-file (vglt:save-plot 'cars-interactive-splom))
```
