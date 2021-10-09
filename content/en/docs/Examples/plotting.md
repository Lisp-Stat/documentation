---
title: "Plotting"
date: 2021-04-26
weight: 3
description: >
  Example plots
---

<!-- For our visualisations -->
<script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>

The plots here show equivalents to the [Vega-Lite example
gallery](https://vega.github.io/vega-lite/examples/).


## Preliminaries

### Load Vega-Lite

Load Vega-Lite and network libraries:

```lisp
(asdf:load-system :lisp-stat)
(asdf:load-system :plot/vglt)
(asdf:load-system :dexador)
(asdf:load-system :access)
```

### Load car data

Many of the examples in this section use the vega-lite version of the
classic `mtcars` data set.

```lisp
(in-package :lisp-stat)
(defparameter vega-cars
  (dfio:vl-to-df
    (dex:get
	  "https://raw.githubusercontent.com/vega/vega-datasets/master/data/cars.json"
	  :want-stream t)))
```

{{< alert title="Note" >}}The plotting system is tested on MS Windows
and MacOS using Chrome. It is known to work on other platforms,
notably Ubuntu/Firefox by using the `:browser :default` option to
`plot-from-file`. This option simply tells the operating system to
open the browser using its built in (XDG) mechanism. See [issue
#2](https://github.com/Lisp-Stat/documentation/issues/2) for more
details. {{</alert >}}

## Histograms

### Basic

For this simple [histogram
example](https://vega.github.io/vega-lite/examples/histogram.html)
we'll use the IMDB film rating data set. Load it into a data frame:

```lisp
(defparameter imdb
  (dfio:vl-to-df
    (dex:get
	  "https://raw.githubusercontent.com/vega/vega-datasets/master/data/movies.json"
	  :want-stream t)))
```

and plot a basic histogram

```lisp
(vglt:plot (vglt:histogram imdb "IMDB-RATING"))
```

{{< vega id="imdb" spec="/docs/examples/imdb-histogram.vg.json" >}}

<!--
And here's the same image as a PNG:

{{< figure src="/docs/examples/imdb-histogram.png" >}}


<div id="imdb2"></div>
<script type="text/javascript">
  var spec = "/docs/examples/imdb-histogram.vg.json"
  vegaEmbed('#imdb2', spec).then(function(result) {
  // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>


### Relative frequency

Relative frequency histogram. The data is binned with first
transform. The number of values per bin and the total number are
calculated in the second and third transform to calculate the relative
frequency in the last transformation step.
-->


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
  (vglt:scatter-plot vega-cars "HORSEPOWER" "MILES-PER-GALLON"))
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
   (alexandria-2:line-up-first
    (vglt:spec)
	(vglt:add "data" `(("values" . ,(dfio:df-to-alist vega-cars))))
	(vglt:add "transform" #((("calculate" . "datum.ORIGIN[0]") ("as" . "OriginInitial"))))
	(vglt:add "mark" "text")
	(vglt:add "encoding" '(("x" ("field" . "HORSEPOWER") ("type" . "quantitative") ("title" . "Horsepower"))
	                       ("y" ("field" . "MILES-PER-GALLON") ("type" . "quantitative") ("title" . "Miles per Gallon"))
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
  (alexandria-2:line-up-first
   (vglt:spec)
   (vglt:add "repeat" '(("row" . #("HORSEPOWER" "ACCELERATION" "MILES-PER-GALLON"))
			            ("column" . #("MILES-PER-GALLON" "ACCELERATION" "HORSEPOWER"))))
   (vglt:add "spec"
             `(("data" ("values" . ,(dfio:df-to-alist vega-cars)))
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

## Strip plot
The Vega-Lite [strip plot
example](https://vega.github.io/vega-lite/examples/tick_strip.html)
shows the relationship between horsepower and the number of cylinders
using tick marks.

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
  (alexandria-2:line-up-first
	(vglt:spec)
    (vglt:add "description" "Shows the relationship between horsepower and the number of cylinders using tick marks.")
	(vglt:add "data" `(("values" . ,(dfio:df-to-alist vega-cars))))
	(vglt:add "mark" "tick")
	(vglt:add "encoding" '(("x" ("field" . "HORSEPOWER") ("type" . "quantitative") ("title" . "Horsepower"))
	                       ("y" ("field" . "CYLINDERS")  ("type" . "ordinal") ("title" . "Cylinders"))))))
(plot:plot-from-file (vglt:save-plot 'cars-strip-plot))
```
