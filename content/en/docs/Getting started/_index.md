---
title: "Getting Started"
linkTitle: "Getting Started"
weight: 2
description: >
  Install to plotting in five minutes
---

## Prerequisites

- SBCL or CCL Common Lisp
- MacOS or Windows 10
- Quicklisp
- Chrome

## Load & Configure

First load Lisp-Stat, plotting libraries and data and configure the
environment. We assume you have already obtained the libraries via a package manager like [clpm](https://common-lisp.net/project/clpm/) or [quicklisp](https://www.quicklisp.org/beta/). See the [installation instructions](https://github.com/Lisp-Stat/lisp-stat) on github.

### Lisp-Stat

```lisp
(asdf:load-system :lisp-stat)
(in-package :ls-user)
```

### Vega-Lite

```lisp
(asdf:load-system :plot/vglt)
(asdf:load-system :dfio/json)
```

### Data

```lisp
(defparameter cars
  (dfio:vl-to-df
    (dex:get
	  "https://raw.githubusercontent.com/vega/vega-datasets/master/data/cars.json"
	  :want-stream t)))
```

<!--
```lisp
(defdf cars
  (dfio:vl-to-df
    (dex:get vglt:cars :want-stream t)))
```
-->



## View

Print the data frame (showing the first 25 rows by default)

```lisp
(pprint cars)
;; ORIGIN YEAR       ACCELERATION WEIGHT_IN_LBS HORSEPOWER DISPLACEMENT CYLINDERS MILES_PER_GALLON NAME
;; USA    1970-01-01         12.0          3504        130        307.0         8             18.0 chevrolet chevelle malibu
;; USA    1970-01-01         11.5          3693        165        350.0         8             15.0 buick skylark 320
;; USA    1970-01-01         11.0          3436        150        318.0         8             18.0 plymouth satellite
;; USA    1970-01-01         12.0          3433        150        304.0         8             16.0 amc rebel sst
;; USA    1970-01-01         10.5          3449        140        302.0         8             17.0 ford torino
;; USA    1970-01-01         10.0          4341        198        429.0         8             15.0 ford galaxie 500
;; USA    1970-01-01          9.0          4354        220        454.0         8             14.0 chevrolet impala
;; USA    1970-01-01          8.5          4312        215        440.0         8             14.0 plymouth fury iii
;; USA    1970-01-01         10.0          4425        225        455.0         8             14.0 pontiac catalina
;; USA    1970-01-01          8.5          3850        190        390.0         8             15.0 amc ambassador dpl
;; Europe 1970-01-01         17.5          3090        115        133.0         4 NIL              citroen ds-21 pallas
;; USA    1970-01-01         11.5          4142        165        350.0         8 NIL              chevrolet chevelle concours (sw)
;; USA    1970-01-01         11.0          4034        153        351.0         8 NIL              ford torino (sw)
;; USA    1970-01-01         10.5          4166        175        383.0         8 NIL              plymouth satellite (sw)
;; USA    1970-01-01         11.0          3850        175        360.0         8 NIL              amc rebel sst (sw)
;; USA    1970-01-01         10.0          3563        170        383.0         8             15.0 dodge challenger se
;; USA    1970-01-01          8.0          3609        160        340.0         8             14.0 plymouth 'cuda 340
;; USA    1970-01-01          8.0          3353        140        302.0         8 NIL              ford mustang boss 302
;; USA    1970-01-01          9.5          3761        150        400.0         8             15.0 chevrolet monte carlo
;; USA    1970-01-01         10.0          3086        225        455.0         8             14.0 buick estate wagon (sw)
;; Japan  1970-01-01         15.0          2372         95        113.0         4             24.0 toyota corona mark ii
;; USA    1970-01-01         15.5          2833         95        198.0         6             22.0 plymouth duster
;; USA    1970-01-01         15.5          2774         97        199.0         6             18.0 amc hornet
;; USA    1970-01-01         16.0          2587         85        200.0         6             21.0 ford maverick                 ..
```

Show the last few rows:

```lisp
(tail cars)
;; ORIGIN YEAR       ACCELERATION WEIGHT_IN_LBS HORSEPOWER DISPLACEMENT CYLINDERS MILES_PER_GALLON NAME
;; USA    1982-01-01         17.3          2950         90          151         4               27 chevrolet camaro
;; USA    1982-01-01         15.6          2790         86          140         4               27 ford mustang gl
;; Europe 1982-01-01         24.6          2130         52           97         4               44 vw pickup
;; USA    1982-01-01         11.6          2295         84          135         4               32 dodge rampage
;; USA    1982-01-01         18.6          2625         79          120         4               28 ford ranger
;; USA    1982-01-01         19.4          2720         82          119         4               31 chevy s-10
```

## Statistics

Look at a few statistics on the data set.

```lisp
(mean cars:acceleration) ; => 15.5197
```

```lisp
LS-USER> (summary cars)
ORIGIN: 254 (63%) x USA, 79 (19%) x Japan, 73 (18%) x Europe,
YEAR: 61 (15%) x 1982-01-01, 40 (10%) x 1973-01-01, 36 (9%) x 1978-01-01, 35 (9%) x 1970-01-01, 34 (8%) x 1976-01-01, 30 (7%) x 1975-01-01, 29 (7%) x 1971-01-01, 29 (7%) x 1979-01-01, 29 (7%) x 1980-01-01, 28 (7%) x 1972-01-01, 28 (7%) x 1977-01-01, 27 (7%) x 1974-01-01,
ACCELERATION: 406 reals, min=8, q25=13.674999999999999d0, q50=15.45d0, q75=17.16666632692019d0, max=24.8d0
WEIGHT-IN-LBS: 406 reals, min=1613, q25=2226, q50=2822.5, q75=3620, max=5140
HORSEPOWER: 400 reals, min=46, q25=75.77778, q50=94.33333, q75=129.57143, max=2306 (1%) x NIL,
DISPLACEMENT: 406 reals, min=68, q25=104.25, q50=147.92307, q75=277.76923, max=455
CYLINDERS: 207 (51%) x 4, 108 (27%) x 8, 84 (21%) x 6, 4 (1%) x 3, 3 (1%) x 5,
MILES-PER-GALLON: 398 reals, min=9, q25=17.33333317438761d0, q50=22.727271751923993d0, q75=29.14999923706055d0, max=46.6d08 (2%) x NIL,
```

Note: The car models, essentially the row names, have been removed
from the summary.

## Plot

Create a scatter plot specification with default values:

```lisp
(defparameter cars-plot (vglt:scatter-plot cars "HORSEPOWER" "MILES-PER-GALLON"))
```

Render the plot:

```lisp
(plot:plot-from-file (vglt:save-plot 'cars-plot))
```

{{< figure src="/docs/examples/vega-cars-scatter-plot.png" title="Horsepower vs. MPG scatter plot" >}}
