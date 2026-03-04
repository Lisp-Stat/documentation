---
title: "Plotting"
date: 2021-02-21
weight: 9
draft: true
description: >
  Visualising data in Lisp-Stat
---

Lisp-Stat can render plots with text or
[Vega-Lite](https://vega.github.io/vega-lite/).  Vega-Lite (VL) is a
browser based plotting system based on a grammar of graphics language.

## Plotting with text

Lisp-Stat includes text based plotting functions that are useful for
visualising data in the REPL.
[cl-spark](https://github.com/tkych/cl-spark) provides this
functionality. The text/histogram function provides text based
histograms. See that function for documentation.

## Plotting with Vega-Lite

Although Vega-Lite can render plots in any browser that supports
JavaScript, we found that the easiest integration is with Chrome, and
we assume here this browser is available.  It would work equally well
in Electron, should someone want to pick up that integration.

### Configuring a browser

You can configure a default browser in the file `browser.lisp` in the
main system directory.  The default is configured for Chrome, and this
is the recommended browser.  Browser command-line options can also be
configured here.

### Vega-Lite specification
Vega-Lite plots are specified with JSON to encoding mappings from data
to the properties of the plot.  In Lisp-Stat, the encodings are
specified as ALISTs, and then transformed to Vega-Lite format with a
JSON library. An ALIST is a convenient format, since this data
structure is built-in to Common Lisp and therefore can be manipulated
with standard functions.

{{< alert title="Note" >}}There are at least eight Common Lisp JSON
libraries ([reviewed here](https://sabracrolleton.github.io/json-review)).  Each has their
own quirks regarding encoding/decoding.  This manual uses [yason](https://github.com/phmarek/yason) for encoding.  [Shasht](https://github.com/yitzchak/shasht) is also
known to work well with Vega-Lite specifications. {{< /alert >}}

The easiest way to see how a Lisp-Stat plot encoding looks is to
*decode* one of the [Vega-Lite examples](https://vega.github.io/vega-lite/examples/).  For example a
[simple bar chart](https://vega.github.io/vega-lite/examples/bar.html)
from the [JSON spec files](https://github.com/vega/vega-lite/tree/master/examples/specs)
looks like this in JSON:

```json
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "A simple bar chart with embedded data.",
  "data": {
    "values": [
      {"a": "A", "b": 28}, {"a": "B", "b": 55}, {"a": "C", "b": 43},
      {"a": "D", "b": 91}, {"a": "E", "b": 81}, {"a": "F", "b": 53},
      {"a": "G", "b": 19}, {"a": "H", "b": 87}, {"a": "I", "b": 52}
    ]
  },
  "mark": "bar",
  "encoding": {
    "x": {"field": "a", "type": "nominal", "axis": {"labelAngle": 0}},
    "y": {"field": "b", "type": "quantitative"}
  }
}
```

and and if we decode this with yason, using:

```lisp
(reverse
 (yason:parse
  (dex:get "https://raw.githubusercontent.com/vega/vega-lite/master/examples/specs/bar.vl.json" :want-stream t)
  :object-as :alist
  :json-arrays-as-vectors t))

```

we get:

```lisp
(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json")
 ("description" . "A simple bar chart with embedded data.")
 ("data"
  ("values"
   . #((("b" . 28) ("a" . "A")) (("b" . 55) ("a" . "B"))
       (("b" . 43) ("a" . "C")) (("b" . 91) ("a" . "D"))
       (("b" . 81) ("a" . "E")) (("b" . 53) ("a" . "F"))
       (("b" . 19) ("a" . "G")) (("b" . 87) ("a" . "H"))
       (("b" . 52) ("a" . "I")))))
 ("mark" . "bar")
 ("encoding" ("y" ("type" . "quantitative") ("field" . "b"))
  ("x" ("axis" ("labelAngle" . 0)) ("type" . "nominal") ("field" . "a"))))
```

We can encode this `alist` back to the original JSON with:

```lisp
(let ((yason:*list-encoder* 'yason:encode-alist))
  (yason:with-output-to-string* ()
    (yason:encode *)))
```

This mechanism is generic, and as you will see, we can build up an
`alist` that corresponds to any Vega-Lite spec by manipulating the
values in the `alist`.  This is what the convenience functions (like
`bar-chart`) in the `vglt` package do.  Most of the time you will be
working with the convenience functions.

### Manipulating the spec

Let's suppose that the width of the chart is too narrow.  The Vega-Lite documentation page for [customizing size](https://vega.github.io/vega-lite/docs/size.html) tells us that adding a 'width' property will let us control this. For this, simply push the property onto the spec.  Assuming that you have saved the specification into a variable named `*plot*`:

```lisp
(pushnew '("width" . 300) *plot*)
```

and you are done.  Sometimes the value you wish to manipulate is a bit
deeper in the specification property hierarchy.  For these cases you can use the
[access](https://github.com/AccelerationNet/access) system, which
provides a convenient mechanism to access these nested values. Say,
for example, you wanted to add an ordering to the bar chart. To [sort
by another encoding channel](https://vega.github.io/vega-lite/docs/sort.html#sort-by-encoding),
you need to add a 'sort' property to one of the channels.  If we want
to sort `x` by the value of the `y` field:

```lisp
(pushnew '("sort" . "-y") (accesses *plot* :encoding :x))
```

You can use Common Lisp functions to retrieve or set values within the
`alist` just like you would any other list to build up the plot
specification.

### Adding data

There are two ways to plot Lisp-Stat data in Vega-Lite:

1. embed the data into the specification
2. write the data to a file and use a data URL

#### Embedding data

To embed the data into the plot specification, use the
`dfio:df-to-alist` function.  This will transform a data frame into an
alist format that can be embedded into the Vega-Lite specification.
For example, let's start with an empty variable `spec`, with only a
schema in it.  Here is how you would add data to it from a data-frame:

```lisp
(setf spec (acons "data" `(("values" . ,(df-to-alist data-frame))) spec))
```

#### Writing data

For larger data sets, you probably want to save the data to a file or
network location and use the Vega-Lite 'url' property in the
specification.  You can write data frames to streams or strings in
Vega-Lite format using the `dfio:df-to-vl` function.  You can also use
the inverse of this function: `dfio:vl-to-df` to read a Vega-Lite data
array into a data-frame.  This is useful for obtaining sample data
sets from the Vega-Lite ecosystem.

### Rendering the plot

There are two steps to rendering a plot:

- saving the specification to a file in HTML and JavaScript format
- calling the browser to render the plot

The first step uses a back-end specific function. For example the
Vega-Lite function for saving a plot is `vglt:save-plot`, the
[Plotly](https://plotly.com/) one (when available), would be
`plty:save-plot`. The browser functionality is common across all
backends that use a browser for rendering, and these are located in
the `plot` package.

This example demonstrates rendering data from the Lisp-Stat [notebook
on categorical variables](/docs/examples/). First some quick
boilerplate to set up the environment:

```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/ips
```

```lisp
(asdf:clear-source-registry)
(asdf:load-system :ips)         ; data examples
(asdf:load-system :plot/vglt)   ; Vega-Lite plotting
(in-package :ips)
(defparameter online (read-csv (dex:get ips::eg01-07 :want-stream t)))
(defparameter online-bar-chart (vglt:bar-chart online "SOURCE" "COUNT"))
```

Now we can render the spec like so:

```lisp
(plot:plot-from-file			        ; Common browser plotting
 (vglt:save-plot 'online-bar-chart))	; Vega-Lite specific save
```

You should see a new Chrome window containing the plot.
