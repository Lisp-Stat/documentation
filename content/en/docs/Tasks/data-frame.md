---
title: "Working with data"
date: 2021-04-28
weight: 2
description: >
  Manipulating data using a data frame
---

## Overview

A common lisp data frame is a collection of observations of sample
variables that shares many of the properties of arrays and lists.  By
design it can be manipulated using the same mechanisms used to
manipulate lisp arrays.  This allow you to, for example, transform a
data frame into an array and use
[array-operations](/docs/tasks/array-operations) to manipulate it, and
then turn it into a data frame again to use in modeling or plotting.

{{< alert title="Note" >}}In this document we refer to _column_ and
_variable_ interchangeably. Likewise _factor_ and _category_ refer to a
variable type. Where necessary we distinguish the terminology.

The examples assume that you are `in-package :LS-USER`. If not, you
will need to use a package prefix if following along.  All the samples
may be copied to the clipboard using the `copy` button in the
upper-right corner of the sample code box.{{</alert >}}

### Load/install

Data-frame is part of the Lisp-Stat package. It can be used
independently if desired. Since the examples in this manual use
Lisp-Stat functionality, we'll use it from there rather than load
independently.

```lisp
(ql:quickload :lisp-stat)
```

Within the Lisp-Stat system, the `LS-USER` package is set-up for
statistics work. Type the following to enter the package:

```lisp
(in-package :ls-user)
```

## Common Lisp Implementation

Data frame is implemented as a two-dimensional common lisp data
structure: a vector of vectors for data, and a hash table mapping
variable names to column vectors.  All columns are of equal length.
This structure provides the flexibility required for column oriented
manipulation, as well as speed for large data sets.

## Data variables

If you're collecting data and exploring a problem domain, you'll
sometimes have a collection of separate variable to start with.
Common Lisp has two structures for holding multiple observations of
variables: _list_ and _vector_, collectively known as a _sequence_.
For the most part a _vector_ is more efficient, and the recommended
way to work with variables that are independent of a data-frame.

### defparameter
Lisp-Stat provides a wrapper over Common Lisp's `defparameter`
function to make working with data variables a little easier.  You can
define a variable with the `def` function. Here are some variables
containing some weather data in Singapore over the last 14 days:

```lisp
(def max-temps '#(30.1 30.3 30.3 30.8 31.6 31.5 32.7 32.1 32.1 31.4 31.9 31.7 32.2 31.1))
(def min-temps '#(24.6 25.4 25.1 24.5 23.7 25.6 24.6 24.7 25.0 25.2 25.1 25.6 25.5 25.2))
(def precipitation '#(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.6 0.4 0.0 0.0 ))
```

For a quick analysis, you can see how this is easier to work with than
a data-frame.

After you have been working for a while you may want to find out what
variables you have defined (using `def`).  The function
`variables` will produce a listing:

```lisp
(variables)
; => (max-temps min-temps precipitation)
```

If you are working with very large variables you may occasionally want
to free up some space by getting rid of some variables you no longer
need.  You can do this using the `undef` function:

```lisp
(undef 'max-temps)
```

To a save variable you can use the `savevar` function.  This function
allows you to save one or more variables into a file.  A new file is
created and any existing file by the same name is destroyed.  To save
the variable `precipitation` in a file called `precipitation.lisp`
type

```lisp
(savevar 'precipitation "precipitation")
```

Do not add the `.lisp` suffix yourself; `savevar` will supply
it. To save the two variables `precipitation` and `min-temps`
in the file `examples.lisp` type:

```lisp
(savevar '(min-temps precipitation) "sg-weather")
```

The files `precipitation.lisp` and `sg-weather.lisp` now contain a set
of expressions that, when read in with the `load` command, will
recreate the variables `precipitation` and `min-temp`.  You can look
at these files with an editor like the Emacs editor and you can
prepare files with your own data by following these examples.


### define-data-frame

The `define-data-frame` macro is conceptually equivalent to the Common
Lisp `defparameter`, but with some additional functionality that makes
working with data frames easier. You use it the same way you'd use
`defparameter`, for example:

```lisp
(define-data-frame foo <any-function returning a data frame>
```

We'll use both ways of defining data frames in this manual. The access
methods that are defined by `define-data-frame` are described in the
[access data](/docs/tasks/data-frame/#access-data) section.

## Create data-frames

A data frame can be created from a Common Lisp `array`, `alist`,
`plist` or individual data vectors.

Data frame columns represent sample set *variables*, and its rows
are *observations* (or cases).

{{< alert title="Note" >}}For these examples we are going to install a modified version of the Lisp-Stat data-frame print-object function. This will cause the REPL to display the data-frame at creation, and save us from having to type (print data-frame) in each example.  If you'd like to install it as we have, use the code below.
{{< /alert >}}


```lisp
(defmethod print-object ((df data-frame) stream)
  "Print the first six rows of DATA-FRAME"
  (let* ((*print-lines* 6)
	     (*print-pretty* t))
    (df:pprint-data-frame df stream)))
(setf *print-pretty* t)
```

Let's create a simple data frame. First we'll setup some example
variables to represent our sample domain:

```lisp
(defparameter v #(1 2 3 4)) ; data vector
(defparameter b #*0110)     ; bits
(defparameter s #(a b c d)) ; symbols (variable names)
(defparameter plist `(:vector ,v :symbols ,s))
```

### From p/a-lists

Now, suppose we want to create a data frame from a `plist`

```lisp
(apply #'df plist)
;; VECTOR SYMBOLS
;;      1 A
;;      2 B
;;      3 C
;;      4 D
```

We could also have used the `plist-df` function:

```lisp
(plist-df plist)
;; VECTOR SYMBOLS
;;      1 A
;;      2 B
;;      3 C
;;      4 D
```

and to demonstrate the same thing using an alist, we'll use the
`alexandria:plist-alist` function to convert the `plist` into an
`alist`:

```lisp
(alist-df (plist-alist plist))
;; VECTOR SYMBOLS
;;      1 A
;;      2 B
;;      3 C
;;      4 D
```

### From vectors
You can use `make-df` to create a data frame from keys and a list of
vectors. Each vector becomes a column in the data-frame.

```lisp
(make-df '(:a :b) '(#(1 2 3) #(10 20 30)))
;; A  B
;; 1 10
;; 2 20
;; 3 30
```

This is useful if you've started working with variables defined with
`def`, `defparameter` or `defvar` and want to combine them into a data
frame.

### From arrays

`matrix-df` converts a matrix (array) to a data-frame with the given
keys.

```lisp
(matrix-df #(:a :b) #2A((1 2)
	                    (3 4)))
;#<DATA-FRAME (2 observations of 2 variables)>
```

This is useful if you need to do a lot of numeric number-crunching on
a data set as an array, perhaps with BLAS or array-operations then
want to add categorical variables and continue processing as a
data-frame.


### Example datasets

Vincent Arel-Bundock maintains a library of nearly 1500 [R
datasets](https://github.com/vincentarelbundock/Rdatasets) that is a
consolidation of example data from various R packages.  The
`lisp-stat/rdata` system allows you to load these to use in Lisp-Stat.
To get started, try loading the classic `mtcars` data set:

```lisp
(ql:quickload :lisp-stat/rdata)
(define-data-frame mtcars
  (read-csv (rdata:rdata 'rdata:datasets 'rdata:mtcars)))
;"MTCARS"
```

You can list the packages in Rdatasets like so:
```lisp
(rdata:show-packages)
;(RDATA:VCD RDATA:TIDYR RDATA:TEXMEX RDATA:SURVIVAL RDATA:STEVEDATA RDATA:STAT2DATA RDATA:SEM RDATA:SANDWICH RDATA:RPART RDATA:ROBUSTBASE RDATA:RESHAPE2 RDATA:QUANTREG RDATA:PSYCH RDATA:PSCL RDATA:PLYR RDATA:PLM RDATA:PALMERPENGUINS RDATA:NYCFLIGHTS13 RDATA:MULTGEE RDATA:MOSAICDATA RDATA:MI RDATA:MEDIATION RDATA:MASS RDATA:LMEC RDATA:LME4 RDATA:LATTICE RDATA:KMSURV RDATA:ISLR RDATA:HWDE RDATA:HSAUR RDATA:HISTDATA RDATA:GT RDATA:GGPLOT2MOVIES RDATA:GGPLOT2 RDATA:GEEPACK RDATA:GAP RDATA:FPP2 RDATA:FORECAST RDATA:EVIR RDATA:ECDAT RDATA:DRC RDATA:DRAGRACER RDATA:DPLYR RDATA:DATASETS RDATA:DAAG COUNT RDATA:CLUSTER RDATA:CARDATA RDATA:BOOT RDATA:AER)
```

and the individual data sets within each package with the
`show-package-items` command.  Here's an example listing the built-in R
data set:

```lisp
(rdata:show-package-items 'rdata:datasets)
```

Here's the first few rows of the table produced by the above
command.

| Dataset               | Title                                                           | Vars. | Obs. |
| ----------------------|-----------------------------------------------------------------|------:|-----:|
| ABILITY.COV           | Ability and Intelligence Tests                                  |     8 |    6 |
| AIRMILES              | Passenger Miles on Commercial US Airlines, 1937-1960            |     2 |   24 |
| AIRPASSENGERS         | Monthly Airline Passenger Numbers 1949-1960                     |     2 |  144 |
| AIRQUALITY            | New York Air Quality Measurements                               |     6 |  153 |
| ANSCOMBE              | Anscombe's Quartet of 'Identical' Simple Linear Regressions     |     8 |   11 |
| ATTENU                | The Joyner-Boore Attenuation Data                               |     5 |  182 |
| ATTITUDE              | The Chatterjee-Price Attitude Data                              |     7 |   30 |
| AUSTRES               | Quarterly Time Series of the Number of Australian Residents     |     2 |   89 |
| BJSALES               | Sales Data with Leading Indicator                               |     2 |  150 |
| BOD                   | Biochemical Oxygen Demand                                       |     2 |    6 |
| CARS                  | Speed and Stopping Distances of Cars                            |     2 |   50 |
| CHICKWEIGHT           | Weight versus age of chicks on different diets                  |     4 |  578 |
| CHICKWTS              | Chicken Weights by Feed Type                                    |     2 |   71 |
| CO2                   | Mauna Loa Atmospheric CO2 Concentration                         |     2 |  468 |


## Export data frames

These next few functions are the reverse of the ones above used to
create them. These are useful when you want to use foreign libraries
or common lisp functions to process the data.

For this section of the manual, we are going to work with a subset of
the `mtcars` data set from above. We'll use the
[select](/docs/tasks/select/) package to take the first 5 rows so that
the data transformations are easier to see.

```lisp
(defparameter mtcars-small (select mtcars (range 0 5) t))
```

The next three functions convert a data-frame to and from standard
common lisp data structures.  This is useful if you've got data in
Common Lisp format and want to work with it in a data frame, or if
you've got a data frame and want to apply Common Lisp operators on it
that don't exist in `df`.

### as-alist

Just like it says on the tin, `as-alist` takes a data frame and
returns an `alist` version of it (formatted here for clearer output --
a pretty printer that outputs an alist in this format would be a
welcome addition to CL/Lisp-Stat)

```lisp
(as-alist mtcars-small)
;; ((MTCARS:X1 . #("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout"))
;;  (MTCARS:MPG . #(21 21 22.8d0 21.4d0 18.7d0))
;;  (MTCARS:CYL . #(6 6 4 6 8))
;;  (MTCARS:DISP . #(160 160 108 258 360))
;;  (MTCARS:HP . #(110 110 93 110 175))
;;  (MTCARS:DRAT . #(3.9d0 3.9d0 3.85d0 3.08d0 3.15d0))
;;  (MTCARS:WT . #(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0))
;;  (MTCARS:QSEC . #(16.46d0 17.02d0 18.61d0 19.44d0 17.02d0))
;;  (MTCARS:VS . #*00110)
;;  (MTCARS:AM . #*11100)
;;  (MTCARS:GEAR . #(4 4 4 3 3))
;;  (MTCARS:CARB . #(4 4 1 1 2)))
```

### as-plist

Similarly, `as-plist` will return a `plist`:

```lisp
(nu:as-plist mtcars-small)
;; (MTCARS:X1 #("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout")
;;  MTCARS:MPG #(21 21 22.8d0 21.4d0 18.7d0)
;;	MTCARS:CYL #(6 6 4 6 8)
;;	MTCARS:DISP #(160 160 108 258 360)
;;	MTCARS:HP #(110 110 93 110 175)
;;	MTCARS:DRAT #(3.9d0 3.9d0 3.85d0 3.08d0 3.15d0)
;;	MTCARS:WT #(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0)
;;	MTCARS:QSEC #(16.46d0 17.02d0 18.61d0 19.44d0 17.02d0)
;;	MTCARS:VS #*00110
;;	MTCARS:AM #*11100
;;	MTCARS:GEAR #(4 4 4 3 3)
;;	MTCARS:CARB #(4 4 1 1 2))
```


### as-array

`as-array` returns the data frame as a row-major two dimensional lisp
array.  You'll want to save the variable names using the
[keys](/docs/tasks/data-frame/#keys) function to make it easy to
convert back (see [matrix-df](#from-arrays)).  One of the reasons you
might want to use this function is to manipulate the data-frame using
[array-operations](/docs/tasks/array-operations).  This is
particularly useful when you have data frames of all numeric values.

```lisp
(defparameter mtcars-keys (keys mtcars)) ; we'll use later
(defparameter mtcars-small-array (as-array mtcars-small))
mtcars-small-array
; #2A(("Mazda RX4" 21 6 160 110 3.9d0 2.62d0 16.46d0 0 1 4 4)
;     ("Mazda RX4 Wag" 21 6 160 110 3.9d0 2.875d0 17.02d0 0 1 4 4)
;     ("Datsun 710" 22.8d0 4 108 93 3.85d0 2.32d0 18.61d0 1 1 4 1)
;     ("Hornet 4 Drive" 21.4d0 6 258 110 3.08d0 3.215d0 19.44d0 1 0 3 1)
;     ("Hornet Sportabout" 18.7d0 8 360 175 3.15d0 3.44d0 17.02d0 0 0 3 2))
```

Our abbreviated `mtcars` data frame is now a two dimensional Common
Lisp array.

### vectors

The `columns` function returns the variables of the data frame as a vector of
vectors:

```lisp
(columns mtcars-small)
; #(#("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout")
;   #(21 21 22.8d0 21.4d0 18.7d0)
;	#(6 6 4 6 8)
;	#(160 160 108 258 360)
;	#(110 110 93 110 175)
;	#(3.9d0 3.9d0 3.85d0 3.08d0 3.15d0)
;	#(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0)
;	#(16.46d0 17.02d0 18.61d0 19.44d0 17.02d0)
;	#*00110
;	#*11100
;	#(4 4 4 3 3)
;	#(4 4 1 1 2))
```

This is a column-major lisp array.

You can also pass a selection to the `columns` function to return
specific columns:

```lisp
(columns mtcars-small 'mtcars:mpg)
; #(21 21 22.8d0 21.4d0 18.7d0)
```

The functions in [array-operations](docs/tasks/array-operations/) are
helpful in further dealing with data frames as vectors and arrays. For
example you could convert this to an array by using
[aops:combine](/docs/tasks/array-operations/#combine) with
`columns`:

```lisp
(combine (columns mtcars-small))
; #2A(("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout")
;     (21 21 22.8d0 21.4d0 18.7d0)
;	  (6 6 4 6 8)
;	  (160 160 108 258 360)
;	  (110 110 93 110 175)
;	  (3.9d0 3.9d0 3.85d0 3.08d0 3.15d0)
;	  (2.62d0 2.875d0 2.32d0 3.215d0 3.44d0)
;	  (16.46d0 17.02d0 18.61d0 19.44d0 17.02d0)
;	  (0 0 1 1 0)
;	  (1 1 1 0 0)
;	  (4 4 4 3 3)
;	  (4 4 1 1 2))
```


## Load data

You can use the `dfio` system to load delimited text files, such as
CSV, into a data frame.

### From strings

Here is a short demonstration of reading from strings:

```lisp
(defparameter *d* (dfio:read-csv
                     (format nil "Gender,Age,Height~@
                                  \"Male\",30,180.~@
                                  \"Male\",31,182.7~@
                                  \"Female\",32,1.65e2")))
```

`dfio` tries to hard to decipher the various number formats sometimes
encountered in CSV files:

```lisp
(select (dfio:read-csv
                 (format nil "\"All kinds of wacky number formats\"~%.7~%19.~%.7f2"))
                t 'all-kinds-of-wacky-number-formats)
; => #(0.7d0 19.0d0 70.0)
```

### From files

We saw above that `dfio` can read from strings, so one easy way to
read from a file is to use the `uiop` system function
`read-file-string`.  We can read one of the example data files
included with Lisp-Stat like this:

```lisp
(read-csv
	(uiop:read-file-string #P"LS:DATASETS;absorption.csv"))
;;    IRON ALUMINUM ABSORPTION 
;;  0   61       13          4
;;  1  175       21         18
;;  2  111       24         14
;;  3  124       23         18
;;  4  130       64         26
;;  5  173       38         26 ..
```

For most data sets, this method will work fine.  If you are working
with large CSV files, you may want to consider using a stream from an
open file so you don't have `uiop` read the whole thing in before
processing it into a data frame:

```lisp
(read-csv #P"LS:DATASETS;absorption.csv")
;;    IRON ALUMINUM ABSORPTION 
;;  0   61       13          4
;;  1  175       21         18
;;  2  111       24         14
;;  3  124       23         18
;;  4  130       64         26
;;  5  173       38         26 ..
```


### From URLs

`dfio` can also read from Common Lisp
[streams](http://www.lispworks.com/documentation/HyperSpec/Body/21_a.htm).
Streams operations can be network or file based.  Here is an example
of how to read the classic Iris data set over the network using the
HTTP client [dexador](https://github.com/fukamachi/dexador).

<!-- Revisit for Lisp-Stat example data sets. Git seems to be adding a
double newline to the data set and this causes problems for cl-csv -->

```lisp
(read-csv
 (dex:get
   "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/iris.csv"
   :want-stream t))
;;     X27 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;;   0   1          5.1         3.5          1.4         0.2 setosa
;;   1   2          4.9         3.0          1.4         0.2 setosa
;;   2   3          4.7         3.2          1.3         0.2 setosa
;;   3   4          4.6         3.1          1.5         0.2 setosa
;;   4   5          5.0         3.6          1.4         0.2 setosa
;;   5   6          5.4         3.9          1.7         0.4 setosa ..
```

{{< alert title="Note" >}}The input delimiter is hard-coded to comma
(CSV) in `dfio`; output delimiters can be specified in the save
function.  This is an inherited behavior and can be changed by
following the example in the `write-csv` function.  In
reality, most text based data we encounter are CSV, and there has not
been a need for other delimiters for input.{{< /alert >}}

### From a database

{{< alert color="warning" >}}
This functionality is currently a work in progress. Expected release date is May 2021.
{{< /alert >}}


## Save data

Data frames can be saved into any delimited text format supported by
[cl-csv](https://github.com/AccelerationNet/cl-csv), or several
flavors of JSON, such as Vega-Lite.  Since the JSON reader/writers are
specific to the plotting applications, they are described in the
[plotting](/docs/tasks/plotting) section.

### To files

To save the `mtcars` data frame to disk, you could use:

```lisp
(write-csv mtcars
		   :stream #P"LS:DATASETS;mtcars.csv"
           :add-first-row t)         ; add column headers
```

to save it as CSV, or to save it to tab-separated values:

```lisp
(write-csv mtcars
	       :separator #\tab
		   :stream #P"LS:DATASETS;mtcars.tsv"
		   :add-first-row t)         ; add column headers
```


### To a database

See the section above, [From a database](/docs/tasks/data-frame/#from-a-database).



## Access data

This section describes various way to access data variables.


### Access a data-frame

Let's use `define-data-frame` to define the `iris` data
frame. We'll use both of these data frames in the examples below.

```lisp
(define-data-frame iris
  (read-csv (rdata:rdata 'rdata:datasets 'rdata:iris)))
COMMON-LISP:WARNING: Missing column name was filled in
"IRIS"
```

We now have a [global
variable](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node67.html)
named `iris` that represents the data frame.  Let's look at the first
part of this data:

```lisp
(head iris)
;;   X29 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;; 0   1          5.1         3.5          1.4         0.2 setosa
;; 1   2          4.9         3.0          1.4         0.2 setosa
;; 2   3          4.7         3.2          1.3         0.2 setosa
;; 3   4          4.6         3.1          1.5         0.2 setosa
;; 4   5          5.0         3.6          1.4         0.2 setosa
;; 5   6          5.4         3.9          1.7         0.4 setosa
```

Notice a couple of things.  First, there is a column `X27`. In fact if
you look back at previous data frame output in this tutorial you will
notice various columns named `X` followed by some number.  This is
because the column was not given a name in the data set, so a name was
generated for it. `X` starts at 1 and increased by 1 each time an
unnamed variable is encountered during your Lisp-Stat session.  The
next time you start Lisp-Stat, numbering will start over from 1 again.
We will see how to clean this up this data frame in the next sections.

The second thing to note is the row numbers on the far left side.
When Lisp-Stat prints a data frame it automatically adds row
numbers. Row and column numbering in Lisp-Stat start at 0.  In R they
start with 1.  Row numbers make it convenient to make selections from
a data frame, but they are not part of the data and cannot be selected
or manipulated.  They only appear when a data frame is printed.

### Access a variable

The `define-data-frame` macro also defines symbol macros that allow
you to refer to a variable by name, for example to refer to the `mpg`
column of mtcars, you can refer to it by the Common Lisp
`package:symbol` convention:

```lisp
mtcars:mpg
;#(21 21 22.8d0 21.4d0 18.7d0 18.1d0 14.3d0 24.4d0 22.8d0 19.2d0 17.8d0 16.4d0 17.3d0 15.2d0 10.4d0 10.4d0 14.7d0 32.4d0 30.4d0 33.9d0 21.5d0 15.5d0 15.2d0 13.3d0 19.2d0 27.3d0 26 30.4d0 15.8d0 19.7d0 15 21.4d0)
```

There is a point of distinction to be made here: the _values_ of `mpg`
and the _column_ `mpg`. For example to obtain the same vector using
the selection/sub-setting package `select` we must refer to the
_column_:

```lisp
(select mtcars t 'mtcars:mpg)
;#(21 21 22.8d0 21.4d0 18.7d0 18.1d0 14.3d0 24.4d0 22.8d0 19.2d0 17.8d0 16.4d0 17.3d0 15.2d0 10.4d0 10.4d0 14.7d0 32.4d0 30.4d0 33.9d0 21.5d0 15.5d0 15.2d0 13.3d0 19.2d0 27.3d0 26 30.4d0 15.8d0 19.7d0 15 21.4d0)
```

Note that with `select` we passed the _symbol_ 'mtcars:mpg (you can
tell it's a symbol because of the quote in front of it).

So, the rule here is, if you want the _value_, refer to it directly,
e.g. `mtcars:mpg`. If you are referring to the _column_, use the
symbol. Data frame operations typically require the symbol, where as
Common Lisp and other packages that take vectors use the direct access
form.

### Package names

The `define-data-frame` macro creates a package with the same name as
the data frame and interns symbols for each column in it. This is how
you can refer to the columns by name.  So far we have referred to
variables (values) with a package prefix. You can also refer to them
without package names by using the Common Lisp `use-package`
command:

```lisp
(use-package 'mtcars)
```

You can now use `mpg` by itself, e.g.

```
(mean mpg) ;; => 20.090625000000003d0
```

To stop using the symbols in the current package, you can `unuse` the
data frame:

```lisp
(unuse-package 'mtcars)
```


## Data-frame operations
These functions operate on data-frames as a whole.

### copy

`copy` returns a newly allocated data-frame with the same values as
the original:

```lisp
(copy mtcars-small)
;;   X1                 MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; 2 Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; 3 Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; 4 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2

```

By default only the keys are copied and the original data remains the
same, i.e. a shallow copy. For a deep copy, use the `copy-array`
function as the key:

```lisp
(copy mtcars-small :key #'copy-array)
;;   X1                 MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; 2 Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; 3 Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; 4 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2

```

Useful when applying destructive operations to the data-frame.

### keys

Returns a vector of the variables in the data frame. The keys are
symbols. Symbol properties describe the variable, for example units.

```lisp
(keys mtcars)
; #(MTCARS:X1 MTCARS:MPG MTCARS:CYL MTCARS:DISP MTCARS:HP MTCARS:DRAT MTCARS:WT MTCARS:QSEC MTCARS:VS MTCARS:AM MTCARS:GEAR MTCARS:CARB)
```

Recall the earlier discussion of `X1` for the column name.


### map-df

`map-df` transforms one data-frame into another, row-by-row. Its
function signature is:

```lisp
(map-df data-frame keys function result-keys) ...
```

It applies _function_ to each row, and returns a data frame with the
_result-keys_ as the column (variable) names.  `keys` is a _list_.
You can also specify the type of the new variables in the
`result-keys` list.

The goal for this example is to transform `df1`:

```lisp
(defparameter df1 (make-df '(:a :b) '(#(2 3 5) #(7 11 13))))
```

into a data-frame that consists of the product of `:a` and `:b`, and a
bit mask of the columns that indicate where the value is <= 30.  First
we'll need a helper for the bit mask:

```lisp
(defun predicate-bit (a b)
  "Return 1 if a*b <= 30, 0 otherwise"
  (if (<= 30 (* a b))
      1
      0))
```

Now we can transform `df1` into our new data-frame, `df2`, with:

```lisp
(defparameter df2 (map-df df1 '(:a :b)
			  (lambda (a b)
			    (vector (* a b) (predicate-bit a b)))
			  '((:p fixnum) (:m bit))))
```
Since it was a parameter assignment, we have to view it manually:

```lisp
(pprint df2)
;;    P M
;; 0 14 0
;; 1 33 1
;; 2 65 1
```

Note how we specified both the new key names and their type.  Here's
an example that transforms the imperial to metric units of `mtcars`:

```lisp
(map-df mtcars '(mtcars:x1 mtcars:mpg mtcars:disp mtcars:hp mtcars:wt)
	(lambda (model mpg disp hp wt)
	  (vector model ;no transformation for model (X1), return as-is
              (/ 235.214583 mpg)
		      (/ disp 61.024)
		      (* hp 1.01387)
		      (/ (* wt 1000) 2.2046)))
	'(:model (:100km/l float) (:disp float) (:hp float) (:kg float)))

```

View the new metric units data frame:

```lisp
(head *)
;;   MODEL                        100KM/L      DISP        HP                 KG
;; 0 Mazda RX4         11.200694000000000 2.6219194 111.52570 1188.4241523222775
;; 1 Mazda RX4 Wag     11.200694000000000 2.6219194 111.52570 1304.0913885215832
;; 2 Datsun 710        10.316429138183594 1.7697955  94.28991 1052.3450509113297
;; 3 Hornet 4 Drive    10.991335717317101 4.2278447 111.52570 1458.3143701206573
;; 4 Hornet Sportabout 12.578320018747911 5.8993187 177.42725 1560.3736961788682
;; 5 Valiant           12.995280903347288 3.6870740 106.45635 1569.4456362729313
```

You might be wondering how we were able to refer to the columns
without the ' (quote); in fact we did, at the beginning of the
list. The lisp reader then reads the contents of the list as symbols.

### rows

`rows` returns the rows of a data frame as a vector of vectors:

```lisp
(rows mtcars-small)
;#(#("Mazda RX4" 21 6 160 110 3.9d0 2.62d0 16.46d0 0 1 4 4)
;  #("Mazda RX4 Wag" 21 6 160 110 3.9d0 2.875d0 17.02d0 0 1 4 4)
;  #("Datsun 710" 22.8d0 4 108 93 3.85d0 2.32d0 18.61d0 1 1 4 1)
;  #("Hornet 4 Drive" 21.4d0 6 258 110 3.08d0 3.215d0 19.44d0 1 0 3 1)
;  #("Hornet Sportabout" 18.7d0 8 360 175 3.15d0 3.44d0 17.02d0 0 0 3 2))
```


### remove duplicates

The `df-remove-duplicates` function will remove duplicate rows. Let's
create a data-frame with duplicates:

```lisp
(defparameter dup (make-df '(a b c) '(#(a1 a1 a3)
                                      #(a1 a1 b3)
									  #(a1 a1 c3))))
DUP
```

Confirm a duplicate row:

```lisp
LS-USER> dup
;; A  B  C
;; A1 A1 A1
;; A1 A1 A1
;; A3 B3 C3
```

Now remove duplicate rows 0 and 1:

```lisp
(df-remove-duplicates dup)
;; A  B  C
;; A1 A1 A1
;; A3 B3 C3
```

<!-- TODO
## Detect missing values
-->


## Column operations

You have seen some of these functions before, and for completeness we
repeat them here.  The remainder of the section covers the remaining
column functions.

To obtain a variable (column) from a data frame, use the `column`
function.  Using `mtcars`, defined in [example
datasets](/docs/tasks/data-frame/#example-datasets) above:

```lisp
(column mtcars-small 'mtcars:mpg)
;; #(21 21 22.8d0 21.4d0 18.7d0)
```

Careful readers will note that we used the `mtcars` accessor, and not
`mtcars-small`. We can do this when referring to a data frame that is
a subset of a larger one.

To get all the columns as a vector, use the `columns` function:

```lisp
(columns mtcars-small)
; #(#("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout")
;   #(21 21 22.8d0 21.4d0 18.7d0)
;	#(6 6 4 6 8)
;	#(160 160 108 258 360)
;	#(110 110 93 110 175)
;	#(3.9d0 3.9d0 3.85d0 3.08d0 3.15d0)
;	#(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0)
;	#(16.46d0 17.02d0 18.61d0 19.44d0 17.02d0)
;	#*00110
;	#*11100
;	#(4 4 4 3 3)
;	#(4 4 1 1 2))
```

You can also return a subset of the columns by passing in a selection:

```lisp
(columns mtcars-small '(mtcars:mpg mtcars:wt))
;; #(#(21 21 22.8d0 21.4d0 18.7d0)
;;   #(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0))
```

### Add columns

There are two 'flavors' of add functions, destructive and
non-destructive.  The latter return a **new** data frame as the
result, and the destructive versions modify the data frame passed as a
parameter.  The destructive versions are denoted with a '!' at the end
of the function name.

To add a single column to a data frame, use the `add-column!`
function.  We'll use a data frame similar to the one used in our
reading data-frames from a string example to illustrate column
operations

```lisp
(defparameter *d* (read-csv
		   (format nil "Gender,Age,Height
                              \"Male\",30,180
                              \"Male\",31,182
                              \"Female\",32,165
	                          \"Male\",22,167
	                          \"Female\",45,170")))
```

```lisp
(pprint *d*)
;;   GENDER AGE HEIGHT
;; 0 Male    30    180
;; 1 Male    31    182
;; 2 Female  32    165
;; 3 Male    22    167
;; 4 Female  45    170
```

and add a 'weight' column to it:

```lisp
(add-column! *d* 'weight #(75.2 88.5 49.4 78.1 79.4))

;;   GENDER AGE HEIGHT WEIGHT
;; 0 Male    30    180   75.2
;; 1 Male    31    182   88.5
;; 2 Female  32    165   49.4
;; 3 Male    22    167   78.1
;; 4 Female  45    170   79.4
```

now that we have weight, let's add a BMI column to it to demonstrate
using a function to compute the new column values:

```lisp
(add-column! *d* 'bmi
	     (map-rows *d* '(height weight)
		       #'(lambda (h w) (/ w (square (/ h 100))))))
;;   SEX    AGE HEIGHT WEIGHT       BMI
;; 0 Female  10    180   75.2 23.209875
;; 1 Female  15    182   88.5 26.717787
;; 2 Male    20    165   49.4 18.145086
;; 3 Female  25    167   78.1 28.003874
;; 4 Male    30    170   79.4 27.474049
```

Now let's add multiple columns destructively using `add-columns!`

```lisp
(add-columns! *d* 'a #(1 2 3 4 5) 'b #(foo bar baz qux quux))

;;   GENDER AGE HEIGHT WEIGHT A B
;; 0 Male    30    180   75.2 1 FOO
;; 1 Male    31    182   88.5 2 BAR
;; 2 Female  32    165   49.4 3 BAZ
;; 3 Male    22    167   78.1 4 QUX
;; 4 Female  45    170   79.4 5 QUUX
```

(I removed the BMI column before creating this data frame to improve
clarity)

### Remove columns

Let's remove the columns `a` and `b` that we just added above with
the `remove-columns` function.  Since it returns a new data frame,
we'll need to assign the return value to `*d*`:

```lisp
(setf *d* (remove-columns *d* '(a b)))
;;   GENDER AGE HEIGHT WEIGHT
;; 0 Male    30    180   75.2
;; 1 Male    31    182   88.5
;; 2 Female  32    165   49.4
;; 3 Male    22    167   78.1
;; 4 Female  45    170   79.4
```

### Rename columns

Sometimes data sources can have variable names that we want to change.
To do this, use the `substitute-key!` function.  This example will
rename the 'gender' variable to 'sex':

```lisp
(substitute-key! *d* 'sex 'gender)
; => #<ORDERED-KEYS WEIGHT, HEIGHT, AGE, SEX>
```

If you used `define-data-frame` to create your data frame, and this is
the recommended way, then use the `replace-key!` macro to rename the
column and update the variable references within the data package.
Let's use this now to rename the `mtcars` `X1` variable to `model`.
First a quick look at the first 2 rows as they are now:

```lisp
(head mtcars 2)
;;   X1                 MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
```

Replace `X1` with `model`:

```lisp
(replace-key! mtcars model x1)
```

check that it worked:

```lisp
(head mtcars 2)
;;   MODEL         MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
```

We can now refer to `mtcars:model`

```lisp
mtcars:model
#("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout"
  "Valiant" "Duster 360" "Merc 240D" "Merc 230" "Merc 280" "Merc 280C"
  "Merc 450SE" "Merc 450SL" "Merc 450SLC" "Cadillac Fleetwood"
  "Lincoln Continental" "Chrysler Imperial" "Fiat 128" "Honda Civic"
  "Toyota Corolla" "Toyota Corona" "Dodge Challenger" "AMC Javelin"
  "Camaro Z28" "Pontiac Firebird" "Fiat X1-9" "Porsche 914-2" "Lotus Europa"
  "Ford Pantera L" "Ferrari Dino" "Maserati Bora" "Volvo 142E")
```

### Replace columns

Columns are "setf-able" places and the simplest way to replace a
column is set the field to a new value.  We'll complement the `sex`
field of `*d*`:

```lisp
(df::setf (df:column *d* 'sex) #("Female" "Female" "Male" "Female" "Male"))
;#("Female" "Female" "Male" "Female" "Male")
```

Note that `df::setf` is not exported.  This is an inherited (from
Tamas Papp, aka 'tkp') behavior and likely because it bypasses checks
on column length.  Use this with caution.

You can also replace a column using two functions specifically for
this purpose.  Here we'll replace the 'age' column with new values:

```lisp
(replace-column *d* 'age #(10 15 20 25 30))
;;   SEX    AGE HEIGHT WEIGHT
;; 0 Female  10    180   75.2
;; 1 Female  15    182   88.5
;; 2 Male    20    165   49.4
;; 3 Female  25    167   78.1
;; 4 Male    30    170   79.4
```

That was a non-destructive replacement, and since we didn't reassign
the value of `*d*`, it is unchanged:

```lisp
LS-USER> *d*
;;   SEX    AGE HEIGHT WEIGHT
;; 0 Female  30    180   75.2
;; 1 Female  31    182   88.5
;; 2 Male    32    165   49.4
;; 3 Female  22    167   78.1
;; 4 Male    45    170   79.4
```

We can also use the destructive version to make a permanent change
instead of `setf`-ing `*d*`:

```lisp
(replace-column! *d* 'age #(10 15 20 25 30))
;;   SEX    AGE HEIGHT WEIGHT
;; 0 Female  10    180   75.2
;; 1 Female  15    182   88.5
;; 2 Male    20    165   49.4
;; 3 Female  25    167   78.1
;; 4 Male    30    170   79.4
```

### Transform columns

There are two functions for column transformations.

#### replace-column
`replace-column` can be used to transform a column by applying a
function. This example will add 20 to each value of the `age` column:

```lisp
(replace-column *d* 'age #'(lambda (x) (+ 20 x)))
;;   SEX    AGE HEIGHT WEIGHT
;; 0 Female  30    180   75.2
;; 1 Female  35    182   88.5
;; 2 Male    40    165   49.4
;; 3 Female  45    167   78.1
;; 4 Male    50    170   79.4
```

`replace-column!` can also apply functions to a column, destructively
modifying the column.

#### map-columns

The `map-columns` functions can be thought of as applying a function
on all the values of each variable as a vector, rather than the
individual rows as `replace-column` does.  To see this, we'll use
functions that operate on vectors, in this case `nu:e+`, which is the
vector addition function for Lisp-Stat.  Let's see this working first:

```lisp
(nu:e+ #(1 1 1) #(2 3 4))
; => #(3 4 5)
```

observe how the vectors were added element-wise. We'll demonstrate
`map-columns` by adding one to each of the numeric columns in the
example data frame:

```lisp
(map-columns (select *d* t '(weight age height))
	     #'(lambda (x)
		     (nu:e+ 1 x)))
;;   WEIGHT AGE HEIGHT 
;; 0   76.2  11    181
;; 1   89.5  16    183
;; 2   50.4  21    166
;; 3   79.1  26    168
;; 4   80.4  31    171
```

recall that we used the non-destructive version of `replace-column`
above, so `*d*` has the original values. Also note the use of `select`
to get the numeric variables from the data frame; `e+` can't add
categorical values like gender/sex.

## Row operations

As the name suggests, row operations operate on each row, or
observation, of a data set.

### count-rows

This function is used to determine how many rows meet a certain
condition.  For example if you want to know how many cars have a MPG
(miles-per-galleon) rating greater than 20, you could use:

```lisp
(count-rows mtcars 'mtcars:mpg #'(lambda (x) (< 20 x)))
; => 14
```

### do-rows

`do-rows` applies a function on selected variables.  The function must
take the same number of arguments as variables supplied.  It is
analogous to [dotimes](http://clhs.lisp.se/Body/m_dotime.htm), but
iterating over data frame rows.  No values are returned; it is purely
for side-effects.  Let's create a new data data-frame to
illustrate row operations:

```lisp
LS-USER> (defparameter *d2*
                       (make-df '(a b) '(#(1 2 3) #(10 20 30))))
*D2*
LS-USER> *d2*
;;   A  B
;; 0 1 10
;; 1 2 20
;; 2 3 30
```

This example uses `format` to illustrate iterating using `do-rows` for
side effect:

```lisp
(do-rows *d2* '(a b) #'(lambda (a b) (format t "~A " (+ a b))))
11 22 33
; No value
```

### map-rows

Where `map-columns` can be thought of as working through the data
frame column-by-column, `map-rows` goes through row-by-row.  Here we
add the values in each row of two columns:

```lisp
(map-rows *d2* '(a b) #'+)
#(11 22 33)
```

Since the length of this vector will always be equal to the data-frame
column length, we can add the results to the data frame as a new
column.  Let's see this in a real-world pattern, subtracting the mean
from a column:

```lisp
(add-column! *d2* 'c
           (map-rows *d2* 'b
                     #'(lambda (x) (- x (mean (select *d2* t 'b))))))
;;   A  B     C
;; 0 1 10 -10.0
;; 1 2 20   0.0
;; 2 3 30  10.0
```

You could also have used `replace-column!` in a similar manner to
replace a column with normalize values.


## Create subsets

This example assume you have saved the Rdataset mentioned above to a
variables name `mtcars`.

### mask-rows

`mask-rows` is similar to `count-rows`, except it returns a bit-vector
for rows matching the predicate.  This is useful when you want to pass
the bit vector to another function, like `select` to retrieve only the
rows matching the predicate.

```lisp
(mask-rows mtcars 'mtcars:mpg #'(lambda (x) (< 20 x)))
; => #*11110001100000000111100001110001
```

to make this into a filter:

```lisp
(defparameter efficient-cars
  (select mtcars (mask-rows mtcars 'mtcars:mpg #'(lambda (x) (< 20 x))) t)
  "Cars with MPG > 20")
```

To view them we'll need to call the `pprint` function directly instead
of using the `print-object` function we installed earlier.  Otherwise,
we'll only see the first 6.

```lisp
(pprint efficient-cars)
;;    MODEL           MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;;  0 Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;;  1 Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;;  2 Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;;  3 Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
;;  4 Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
;;  5 Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
;;  6 Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
;;  7 Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
;;  8 Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
;;  9 Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
;; 10 Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
;; 11 Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
;; 12 Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
;; 13 Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```

You can mask multiple rows at the same time by using a predicate
function that accepts the same number of arguments as rows you wish to
mask.

### The select system
`select` is a domain specific language (DSL) for slicing & dicing two
dimensional data structures, including arrays and data frames.  With
`select` you can create data subsets by range, with sequence
specifiers, bit masks and predicates.  The
[select user manual](/docs/tasks/select/) documents this DSL.

For some additional examples of selecting columns, see [manipulating
columns](#manipulate-columns).

<!--
## Sorting and ordering

## Reshaping a data frame

## Expanding a data set

## Merging data frames
-->


## Summarising data

Often the first thing you'll want to do with a data frame is get a
quick summary.  You can do that with these functions, and we've seen
most of them used in this manual.  For more information about these
functions, see the [reference](/docs/reference/) section.

nrow *data-frame*
: return the number of rows in *data-frame*

ncol *data-frame*
: return the number of columns in *data-frame*

dims *data-frame*
: return the dimensions of *data-frame* as a list in (*rows* *columns*) format

keys *data-frame*
: return a vector of symbols representing column names

column-names *data-frame*
: returns a list of strings of the column names in *data-frames*

head *data-frame* &optional *n*
: displays the first *n* rows of data-frame. *n* defaults to 6.

head *data-frame* &optional *n*
: displays the last *n* rows of data-frame. *n* defaults to 6.

### summary

summary *data-frame*
: returns a summary of the variables in *data-frame*

```lisp
  MTCARS:MPG
             32 reals, min=10.4d0, q25=15.399999698003132d0, q50=19.2d0,
             q75=22.8d0, max=33.9d0
  MTCARS:CYL
             14 (44%) x 8, 11 (34%) x 4, 7 (22%) x 6
  MTCARS:DISP
              32 reals, min=71.1d0, q25=120.65d0, q50=205.86666333675385d0,
              q75=334.0, max=472
  MTCARS:HP
            32 reals, min=52, q25=96.0, q50=123, q75=186.25, max=335
  MTCARS:DRAT
              32 reals, min=2.76d0, q25=3.08d0, q50=3.6950000000000003d0,
              q75=3.952000046730041d0, max=4.93d0
  MTCARS:WT
            32 reals, min=1.513d0, q25=2.5425d0, q50=3.325d0,
            q75=3.6766665957371387d0, max=5.424d0
  MTCARS:QSEC
              32 reals, min=14.5d0, q25=16.884999999999998d0, q50=17.71d0,
              q75=18.9d0, max=22.9d0
  MTCARS:VS bits, ones: 14 (44%)
  MTCARS:AM bits, ones: 13 (41%)
  MTCARS:GEAR
              15 (47%) x 3, 12 (38%) x 4, 5 (16%) x 5
  MTCARS:CARB
              10 (31%) x 4,
              10 (31%) x 2,
              7 (22%) x 1,
              3 (9%) x 3,
              1 (3%) x 6,
              1 (3%) x 8>
```

Note that the `model` column, essentially `row-name` was deleted from
the output when writing this manual.  If the column had been named
`row-name`, this would have happened automatically.


## Missing values

Data sets often contain missing values and we need to both understand
where and how many are missing, and how to transform or remove them
for downstream operations.  In Lisp-Stat, missing values are
represented by the keyword symbol `:na`.  You can control this
encoding during delimited text import by passing an `a-list`
containing the mapping.  By default this is a keyword parameter
`map-alist`:

```lisp
(map-alist '(("" . :na)
             ("NA" . :na)))
```

The default maps blank cells ("") and ones containing "NA" to the
missing value keyword `:na`.  Some systems encode missing values as
numeric, e.g. `99`; in this case you can pass in a `map-alist` that
includes this mapping:

```lisp
(map-alist '(("" . :na)
             ("NA" . :na)
			 (99 . :na)))
```


We will use the R air-quality dataset to illustrate working with
missing values.  Let's load it now:

```lisp
(define-data-frame aq
  (read-csv (rdata:rdata 'rdata:datasets 'rdata:airquality)))
```

### Examine

To see missing values we use the predicate `missingp`. This works on
sequences, arrays and data-frames.  It returns a logical sequence,
array or data-frame indicating which values are missing.  `T`
indicates a missing value, `NIL` means the value is present.  Here's
an example of using `missingp` on a vector:

```lisp
(missingp #(1 2 3 4 5 6 :na 8 9 10))
;#(NIL NIL NIL NIL NIL NIL T NIL NIL NIL)
```

and on a data-frame:

```lisp
 (pprint (missingp aq))

;;     X3  OZONE SOLAR-R WIND TEMP MONTH DAY
;;   0 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   1 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   2 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   3 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   4 NIL     T       T NIL  NIL  NIL   NIL
;;   5 NIL   NIL       T NIL  NIL  NIL   NIL
;;   6 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   7 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   8 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;   9 NIL     T     NIL NIL  NIL  NIL   NIL
;;  10 NIL   NIL       T NIL  NIL  NIL   NIL
;;  11 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  12 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  13 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  14 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  15 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  16 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  17 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  18 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  19 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  20 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  21 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  22 NIL   NIL     NIL NIL  NIL  NIL   NIL
;;  23 NIL   NIL     NIL NIL  NIL  NIL   NIL ..
```

We can see that the `ozone` variable contains some missing values.  To
see which rows of `ozone` are missing, we can use the `which`
function:

```lisp
(which aq:ozone :predicate #'missingp)
;#(4 9 24 25 26 31 32 33 34 35 36 38 41 42 44 45 51 52 53 54 55 56 57 58 59 60 64 71 74 82 83 101 102 106 114 118 149)
```

and to get a count, use the `length` function on this vector:

```lisp
(length *) ; => 37
```

It's often convenient to use the `summary` function to get an overview
of missing values.  We can do this because the `missingp` function is
a transformation of a data-frame that yields another data-frame of
boolean values:

```lisp
(summary (missingp aq))
;#<DATA-FRAME (7 x 153)
;  AQ:X3
;        153 (100%) x NIL
;  AQ:OZONE
;           116 (76%) x NIL, 37 (24%) x T
;  AQ:SOLAR-R
;             146 (95%) x NIL, 7 (5%) x T
;  AQ:WIND
;          153 (100%) x NIL
;  AQ:TEMP
;          153 (100%) x NIL
;  AQ:MONTH
;           153 (100%) x NIL
;  AQ:DAY
;         153 (100%) x NIL>
```

we can see that `ozone` is missing 37 values, 24% of the total, and
`solar-r` is missing 7 values.

### Exclude

To exclude missing values from a single column, use the Common Lisp
`remove` function:

```lisp
(remove :na aq:ozone)
;#(41 36 12 18 28 23 19 8 7 16 11 14 18 14 34 6 30 11 1 11 4 32 ...
```

To ensure that our data-frame includes only complete observations, we
exclude any row with a missing value. To do this use the
`drop-missing` function:

```lisp
(head (drop-missing aq))
;;   X3 OZONE SOLAR-R WIND TEMP MONTH DAY
;; 0  1    41     190  7.4   67     5   1
;; 1  2    36     118  8.0   72     5   2
;; 2  3    12     149 12.6   74     5   3
;; 3  4    18     313 11.5   62     5   4
;; 4  7    23     299  8.6   65     5   7
;; 5  8    19      99 13.8   59     5   8
```

### Replace

To replace missing values we can use the transformation functions.
For example we can recode the missing values in `ozone` by the mean.
Let's look at the first six rows of the air quality data-frame:

```lisp
(head aq)
;;   X3 OZONE SOLAR-R WIND TEMP MONTH DAY
;; 0  1    41     190  7.4   67     5   1
;; 1  2    36     118  8.0   72     5   2
;; 2  3    12     149 12.6   74     5   3
;; 3  4    18     313 11.5   62     5   4
;; 4  5    NA      NA 14.3   56     5   5
;; 5  6    28      NA 14.9   66     5   6
```

Now replace `ozone` with the mean using the common lisp function
`nsubstitute`:

```lisp
(nsubstitute (mean (remove :na aq:ozone)) :na aq:ozone)
```

and look at `head` again:

```lisp
(head aq)
;;   X3             OZONE SOLAR-R WIND TEMP MONTH DAY
;; 0  1           41.0000     190  7.4   67     5   1
;; 1  2           36.0000     118  8.0   72     5   2
;; 2  3           12.0000     149 12.6   74     5   3
;; 3  4           18.0000     313 11.5   62     5   4
;; 4  5           42.1293      NA 14.3   56     5   5
;; 5  6           28.0000      NA 14.9   66     5   6
```

You could have used the non-destructive `substitute` if you wanted to
create a new data-frame and leave the original `aq` untouched.

Normally we'd round `mean` up to be consistent, but did not here so
you can see the values that were replaced.


## Dates & times

There are several libraries for [working with
time](https://www.cliki.net/time).  Of these,
[local-time](https://github.com/dlowe-net/local-time) is probably the
best designed and supported and the one we recommend for using with
Lisp-Stat.  It builds on the basic date & time functions included in
Common Lisp and allows you to:

- print timestamp in various standard or custom formats (e.g. RFC1123 or RFC3339)
- parse time strings,
- perform time arithmetic,
- convert Unix times, timestamps, and universal times to and fro.

`local-time` is available in Quicklisp.


<!--

Some of this is probably going to be illustrated in the examples.  Do
we need to cover it here if it's not part of Lisp-Stat?

## Strings




## Categorical variables

https://forcats.tidyverse.org/
-->

<!--
## Best practices

### Packaging data frames

Describe using CL packages to keep multiple data frames and
experiments separated.

-->
