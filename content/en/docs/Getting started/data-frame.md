---
title: "Data Frame"
date: 2021-04-26
weight: 2
description: >
  Getting started with data frames
---

## Load data
We will use one of the [example data sets from R](https://lisp-stat.dev/docs/tasks/data-frame/#example-datasets),
[mtcars](https://vincentarelbundock.github.io/Rdatasets/doc/datasets/mtcars.html),
for these examples. First, load Lisp-Stat and the R data libraries,
and switch into the Lisp-Stat package:

```lisp
(ql:quickload :lisp-stat)
(ql:quickload :lisp-stat/rdata)
(in-package   :ls-user)
```

Now define the data frame, naming it `mtcars`:

```lisp
(define-data-frame mtcars
	(read-csv (rdata:rdata 'rdata:datasets 'rdata:mtcars)))
;;WARNING: Missing column name was filled in
;;#<DATA-FRAME (32 observations of 11 variables)>
```

This macro defines a global variable named `mtcars` and sets up some
convenience functions.

## Examine data

Lisp-Stat's printing system is integrated with the [Common Lisp Pretty
Printing](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node253.html)
facility. By default Lisp-Stat sets `*print-pretty*` to `nil`.

### Basic information
Type the name of the data frame at the REPL to get a simple one-line
summary.

```lisp
mtcars ;; => #<DATA-FRAME (32 observations of 12 variables)>
```

### Printing data

By default, `head` returns the first 6 rows:

```lisp
(head mtcars)
;;   X1                 MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; 2 Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; 3 Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; 4 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
;; 5 Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

and `tail` the last 6 rows:

```lisp
;;   X1              MPG CYL  DISP  HP DRAT    WT QSEC VS AM GEAR CARB
;; 0 Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
;; 1 Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
;; 2 Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
;; 3 Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
;; 4 Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8
;; 5 Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.6  1  1    4    2
```

`pprint` can be used to print the whole data frame:

```lisp
(pprint mtcars)

;;    X1                   MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;;  0 Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;;  1 Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;;  2 Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;;  3 Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
;;  4 Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
;;  5 Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
;;  6 Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
;;  7 Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
;;  8 Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
;;  9 Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
;; 10 Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
;; 11 Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
;; 12 Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
;; 13 Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
;; 14 Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
;; 15 Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
;; 16 Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
;; 17 Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
;; 18 Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
;; 19 Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
;; 20 Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
;; 21 Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
;; 22 AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
;; 23 Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 ..
```

The two dots ".." at the end indicate that output has been
truncated. Lisp-Stat sets the default for pretty printer
`*print-lines*` to 25 rows and output more than this is truncated. If
you'd like to print all rows, set this value to `nil`.

Notice the column named `X1`. This is the name given to the column by
the import function. Note the warning that was issued during the
import. Missing columns are named X1, X2, ..., Xn in increasing order
for the duration of the Lisp-Stat session.

This column is actually the row name, so we'll rename it:

```lisp
(replace-key mtcars row-name x1)
```

and view the results

```lisp
(head mtcars)
;;   ROW-NAME           MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; 2 Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; 3 Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; 4 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
;; 5 Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

### Column names

To see the names of the columns, use the `column-names` function:

```lisp
(column-names mtcars)
;; => ("ROW-NAMES" "MPG" "CYL" "DISP" "HP" "DRAT" "WT" "QSEC" "VS" "AM" "GEAR" "CARB")
```

### Dimensions

We saw the dimensions above in basic information. That was a printed
for human consumption. To get the values in a form suitable for
passing to other functions, use the `dims` command:

```
(aops:dims mtcars) ;; => (32 12)
```

Common Lisp specifies dimensions in row-column order, so `mtcars` has
32 rows and 12 columns.

{{< alert title="Note" >}} Lisp-Stat generally follows the [tidyverse](https://www.tidyverse.org/) philosophy when it comes to row names. By definition, row names are unique, so there is no point including them in a statistical analysis.  Nevertheless, many data sets include row names, so we include some special handling for a column named "row-name".  A column with this name is excluded by default from summaries (and you can include it if you wish).  There is no concept of independent row names as with a R data frame.  A Lisp-Stat data frame is more like a [tibble](https://tibble.tidyverse.org/).
{{< /alert >}}

## Basic Statistics

### Minimum & Maximum

To get the minimum or maximum of a column, say `mpg`, you can use several
Common Lisp methods.  Let's see what `mpg` looks like by typing
the name of the column into the REPL:

```lisp
 mtcars:mpg
;; => #(21 21 22.8d0 21.4d0 18.7d0 18.1d0 14.3d0 24.4d0 22.8d0 19.2d0 17.8d0 16.4d0 17.3d0 15.2d0 10.4d0 10.4d0 14.7d0 32.4d0 30.4d0 33.9d0 21.5d0 15.5d0 15.2d0 13.3d0 19.2d0 27.3d0 26 30.4d0 15.8d0 19.7d0 15 21.4d0)
```

You could, for example, use something like this to find the minimum:

```lisp
(reduce #'min mtcars:mpg) ;; => 10.4d0
```

or the Lisp-Stat function `sequence-maximum` to find the maximum

```lisp
(sequence-maximum mtcars:mpg) ;; => 33.9d0
```

or perhaps you'd prefer
[alexandria:extremum](https://www.crategus.com/books/alexandria/pages/alexandria.0.dev_fun_extremum.html),
a general-purpose tool to find the minimum in a different way:

```lisp
(extremum mtcars:mpg #'<) ;; => 10.4d0
```

The important thing to note is that `mtcars:mpg` is a standard Common
Lisp vector and you can manipulate it like one.

### Mean & standard deviation

```lisp
(mean mtcars:mpg) ;; => 20.090625000000003d0
```

```lisp
(standard-deviation mtcars:mpg) ;; => 5.932029552301219d0
```

### Summarise

You can summarise a column with the `column-summary` function:

```lisp
(column-summary mtcars:mpg)
;; => 32 reals, min=10.4d0, q25=15.399999698003132d0, q50=19.2d0, q75=22.8d0, max=33.9d0
```

or the entire data frame:

```lisp
(summary mtcars)
#<DATA-FRAME (12 x 32)
  MTCARS:CARB
              10 (31%) x 4,
              10 (31%) x 2,
              7 (22%) x 1,
              3 (9%) x 3,
              1 (3%) x 6,
              1 (3%) x 8
  MTCARS:GEAR
              15 (47%) x 3, 12 (38%) x 4, 5 (16%) x 5
  MTCARS:AM bits, ones: 13 (41%)
  MTCARS:VS bits, ones: 14 (44%)
  MTCARS:QSEC
              32 reals, min=14.5d0, q25=16.884999999999998d0, q50=17.71d0,
              q75=18.9d0, max=22.9d0
  MTCARS:WT
            32 reals, min=1.513d0, q25=2.5425d0, q50=3.325d0,
            q75=3.6766665957371387d0, max=5.424d0
  MTCARS:DRAT
              32 reals, min=2.76d0, q25=3.08d0, q50=3.6950000000000003d0,
              q75=3.952000046730041d0, max=4.93d0
  MTCARS:HP
            32 reals, min=52, q25=96.0, q50=123, q75=186.25, max=335
  MTCARS:DISP
              32 reals, min=71.1d0, q25=120.65d0, q50=205.86666333675385d0,
              q75=334.0, max=472
  MTCARS:CYL
             14 (44%) x 8, 11 (34%) x 4, 7 (22%) x 6
  MTCARS:MPG
             32 reals, min=10.4d0, q25=15.399999698003132d0, q50=19.2d0,
             q75=22.8d0, max=33.9d0
```

Recall that a column named `row-name` is treated specially, notice
that it is not included in the summary. You can see why it's excluded
by examining the column's summary:

```lisp
(pprint (column-summary mtcars:row-name))
1 (3%) x "Mazda RX4",
1 (3%) x "Mazda RX4 Wag",
1 (3%) x "Datsun 710",
1 (3%) x "Hornet 4 Drive",
1 (3%) x "Hornet Sportabout",
1 (3%) x "Valiant",
1 (3%) x "Duster 360",
1 (3%) x "Merc 240D",
1 (3%) x "Merc 230",
1 (3%) x "Merc 280",
1 (3%) x "Merc 280C",
1 (3%) x "Merc 450SE",
1 (3%) x "Merc 450SL",
1 (3%) x "Merc 450SLC",
1 (3%) x "Cadillac Fleetwood",
1 (3%) x "Lincoln Continental",
1 (3%) x "Chrysler Imperial",
1 (3%) x "Fiat 128",
1 (3%) x "Honda Civic",
1 (3%) x "Toyota Corolla",
1 (3%) x "Toyota Corona",
1 (3%) x "Dodge Challenger",
1 (3%) x "AMC Javelin",
1 (3%) x "Camaro Z28", ..
```

Columns with unique values in each row aren't very interesting.

## "Use" a data frame

By `use`-ing a data frame package you can avoid the use of the
package qualifier symbol `:` and directly refer to the variable
name. This is similar to R's `attach` function.

```lisp
(use-package 'mtcars)
```

```lisp
(mean mpg) ;; => 20.090625000000003d0
```

the `unuse-package` function stops using the symbols from the
data-frame.

```lisp
(unuse-package 'mtcars)
```

## Saving data

To save a data frame to a CSV file, use the `data-frame-to-csv`
method. Here we save `mtcars` into the Lisp-Stat datasets directory,
including the column names:

```lisp
(data-frame-to-csv mtcars
		           :stream #P"LS:DATASETS;mtcars.csv"
		           :add-first-row t)
```
