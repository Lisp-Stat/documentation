---
title: "Working with data"
date: 2021-2-21
weight: 2
description: >
  Manipulating data using a data frame
---

## Overview

A data frame is a collection of observations of sample variables that
shares many of the properties of arrays and lists.  By design it can be
manipulated using the same mechanisms used to manipulate lisp arrays.
This allow you to, for example, transform a data frame into an array
and use [array-operations](/docs/tasks/array-operations) to manipulate
it, and then turn it into a data frame again to use in modeling or
plotting.

{{< alert title="Note" >}}In this document we refer to _column_ and
_variable_ interchangeably. Likewise _factor_ and _category_ refer to a
variable type. Where necessary we distinguish the terminology.

The examples assume that you are `in-package` `data-frame`. If not, you
will need to use a package prefix if following along.  All the samples
may be copied to the clipboard using the `copy` button in the
upper-right corner of the sample code box.{{</alert >}}


## Implementation

Data frame is implemented as a two-dimensional data structure: a
vector of vectors for data, and a hash table mapping variable names to
column vectors.  All columns are of equal length. This structure
provides both the flexibility required for column oriented
manipulation, as well as the speed for large data sets.

## Data variables

If you're collecting data and exploring a problem domain, you'll often
have a collection of separate variable to start with.  Common Lisp has
two structures for holding variables: _list_ and _vector_,
collectively known as a _sequence_.  For the most part a _vector_ is
more efficient, and the recommended way to work with variables that
are independent of a data-frame.

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


## Creating a data-frame

A data frame can be created from a Common Lisp `array`, `alist` or
`plist`. You can also create a matrix from individual vectors of data.

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
(in-package :ls-user)
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

### Example datasets

Vincent Arel-Bundock maintains a library of nearly 1500 [R
datasets](https://github.com/vincentarelbundock/Rdatasets) that is a
consolidation of example data from various R packages.  The
`lisp-stat/rdata` system allows you to load these to use in Lisp-Stat.
To get started, try loading the classic `mtcars` data set:

```lisp
(ql:quickload :lisp-stat/rdata)
(rdata:load-df 'rdata::datasets 'rdata::mtcars)
;;                      MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB 
;; Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;; Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;; Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;; Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
;; Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
;; Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 ..
```

You can list the packages in Rdatasets like so:
```lisp
(rdata:show-packages)
(RDATA::VCD RDATA::TIDYR RDATA::TEXMEX RDATA::SURVIVAL RDATA::STEVEDATA
 RDATA::STAT2DATA RDATA::SEM RDATA::SANDWICH RDATA::RPART RDATA::ROBUSTBASE
 RDATA::RESHAPE2 RDATA::QUANTREG RDATA::PSYCH RDATA::PSCL RDATA::PLYR
 RDATA::PLM RDATA::PALMERPENGUINS RDATA::NYCFLIGHTS13 RDATA::MULTGEE
 RDATA::MOSAICDATA RDATA::MI RDATA::MEDIATION RDATA::MASS RDATA::LMEC
 RDATA::LME4 RDATA::LATTICE RDATA::KMSURV RDATA::ISLR RDATA::HWDE RDATA::HSAUR
 RDATA::HISTDATA RDATA::GT RDATA::GGPLOT2MOVIES RDATA::GGPLOT2 RDATA::GEEPACK
 RDATA::GAP RDATA::FPP2 RDATA::FORECAST RDATA::EVIR RDATA::ECDAT RDATA::DRC
 RDATA::DRAGRACER RDATA::DPLYR RDATA::DATASETS RDATA::DAAG COUNT RDATA::CLUSTER
 RDATA::CARDATA RDATA::BOOT RDATA::AER)
```

and the individual data sets within each package with the
`show-package-items` command.  Here's an example listing the built-in R
data set:

```lisp
(rdata:show-package-items 'rdata::datasets)
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


## Data-frame operations

These functions operate on data-frames as a whole.  For this section
of the manual, we are going to work with a subset of the `mtcars` data
set from above. We'll use the [select](/docs/tasks/select/) package to
take the first 5 rows so that the data transformations are easier to
see.

```lisp
(defparameter mtcars-small (select mtcars (range 0 5) t))
```

The next three functions convert a data-frame to and from standard
lisp data structures.  This is useful if you've got data in Common
Lisp format and want to work with it in a data frame, or if you've got
a data frame and want to apply Common Lisp operators on it that don't
exist in `df`.

### as-alist

Just like it says on the tin, `as-alist` takes a data frame and
returns an `alist` version of it (formatted for clearer output -- a
pretty printer that outputs an alist in this format would be a welcome
addition to CL)

```lisp
(as-alist mtcars-small)
;; ((:|| . #("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout"))
;;  (:MPG . #(21 21 22.8d0 21.4d0 18.7d0))
;;  (:CYL . #(6 6 4 6 8))
;;  (:DISP . #(160 160 108 258 360))
;;  (:HP . #(110 110 93 110 175))
;;  (:DRAT . #(3.9d0 3.9d0 3.85d0 3.08d0 3.15d0))
;;  (:WT . #(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0))
;;  (:QSEC . #(16.46d0 17.02d0 18.61d0 19.44d0 17.02d0))
;;  (:VS . #*00110)
;;  (:AM . #*11100)
;;  (:GEAR . #(4 4 4 3 3))
;;  (:CARB . #(4 4 1 1 2)))
```

### as-plist

Similarly, `as-plist` will return a `plist`:

```lisp
(nu:as-plist mtcars-small)
;; (:|| #("Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" "Hornet Sportabout")
;;  :MPG #(21 21 22.8d0 21.4d0 18.7d0)
;;	:CYL #(6 6 4 6 8)
;;	:DISP #(160 160 108 258 360)
;;	:HP #(110 110 93 110 175)
;;	:DRAT #(3.9d0 3.9d0 3.85d0 3.08d0 3.15d0)
;;	:WT #(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0)
;;	:QSEC #(16.46d0 17.02d0 18.61d0 19.44d0 17.02d0)
;;	:VS #*00110
;;	:AM #*11100
;;	:GEAR #(4 4 4 3 3)
;;	:CARB #(4 4 1 1 2))
```


### as-array

`as-array` returns the data frame as a row-major two dimensional lisp
array.  You'll want to save the variable names using the
[keys](/docs/tasks/data-frame/#keys) function to make it easy to
convert back (see [matrix-df](#matrix-df)).  One of the reasons you
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

### columns

This function returns the variables of the data frame as a vector of
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
(columns mtcars :mpg)
; #(21 21 22.8d0 21.4d0 18.7d0)
```



### copy

`copy` returns a newly allocated data-frame with the same values as
the original:

```lisp
(copy mtcars-small)
;;                    MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB 
;; Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2

```

Useful when applying destructive operations to the data-frame.

### keys

Returns a vector of the variables in the data frame. The keys are
symbols. Symbol properties describe the variable, for example units.

```lisp
(keys mtcars)
; => #(:|| :MPG :CYL :DISP :HP :DRAT :WT :QSEC :VS :AM :GEAR :CARB)
```

If you are wondering about the `:||` at the start, this means an empty
symbol in the keyword package.  It is used to designate row names.


### map-df

`map-df` transforms one data-frame into another, row-by-row. Its
function signature is:

```lisp
(map-df (data-frame keys function result-keys) ...
```

It applies _function_ to each row, and returns a data frame with the
_result-keys_ as the column (variable) names.  You can also specify the
type of the new variables.

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

```lisp
df2
;#<DATA-FRAME (2 x 3)
;  :P #(14 33 65)
;  :M #*011>
```

Note how we specified both the new key names and their type.  Here's
an example that transforms the imperial to metric units of `mtcars`:

```lisp
(map-df mtcars-small '(:|| :mpg :disp :hp :wt)
	(lambda (model mpg disp hp wt)
	  (vector model
              (/ 235.214583 mpg)
		      (/ disp 61.024)
		      (* hp 1.01387)
		      (/ (* wt 1000) 2.2046)))
	'(:model (:100km/l float) (:disp float) (:hp float) (:wt float)))
;; MODEL                        100KM/L      DISP        HP                 WT
;; Mazda RX4         11.200694000000000 2.6219194 111.52570 1190.9090650968321
;; Mazda RX4 Wag     11.200694000000000 2.6219194 111.52570 1306.8181534936612
;; Datsun 710        10.316429138183594 1.7697955  94.28991 1054.5454316887979
;; Hornet 4 Drive    10.991335717317101 4.2278447 111.52570 1461.3636046894333
;; Hornet Sportabout 12.578320018747911 5.8993187 177.42725 1563.6363297454589
```



### matrix-df

Convert a matrix (array) to a data-frame with the given keys.  Here
we'll use the values we saved earlier to construct a data-frame
equivalent to `mtcars-small`:

```lisp
(matrix-df mtcars-keys  mtcars-small-array)
;;                    MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
```

This is useful if you need to do a lot of numeric number-crunching on
a data set as an array, then want to add categorical variables and
continue processing as a data-frame.


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

## Reading data

You can use the `dfio` system to read text-base tabular data, such as
CSV, into a data frame. Here is a short demonstration of reading from
strings:

```lisp
(defparameter *d* (dfio:csv-to-data-frame
                     (format nil "Gender,Age,Height~@
                                  \"Male\",30,180.~@
                                  \"Male\",31,182.7~@
                                  \"Female\",32,1.65e2")))
```

`dfio` tries to hard to decipher the various number formats sometimes
encountered in CSV files:

```lisp
(select (dfio:csv-to-data-frame
                 (format nil "\"All kinds of wacky number formats\"~%.7~%19.~%.7f2"))
                t :all-kinds-of-wacky-number-formats)
; => #(0.7d0 19.0d0 70.0)
```



### From files

We saw above that `dfio` can read from strings, so one easy way to
read from a file is to use the `uiop` system function
`read-file-string`.  We can read one of the example data files
included with Lisp-Stat like this:

```lisp
(csv-to-data-frame
	(uiop:read-file-string #P"LS:DATASETS;absorbtion.csv"))
;; IRON ALUMINUM ABSORBTION 
;;   61       13          4
;;  175       21         18
;;  111       24         14
;;  124       23         18
;;  130       64         26
;;  173       38         26 ..
```

For most data sets, this method will work fine.  If you are working
with extremely large CSV files, you may want to consider using a
stream from an open file so you don't have `uiop` read the whole thing
in before processing it into a data frame.


### From URLs

`dfio` can also read from Common Lisp
[streams](http://www.lispworks.com/documentation/HyperSpec/Body/21_a.htm).
Streams operations can be network or file based.  Here is an example
of how to read the same file over the network using the HTTP client
[dexador](https://github.com/fukamachi/dexador).

<!-- Revisit this example. There was a trailing white space in the
file that was fixed and pushed to the github repo, but still reports
the same error. No error when loading locally, so I suspect some kind
of proxy or cache in the way. -->

```lisp
(csv-to-data-frame
 (dex:get
   "https://github.com/Lisp-Stat/lisp-stat/blob/77ad465a6a9fc452a1d64ca64ac56d05017014ff/datasets/absorbtion.csv"
   :want-stream t))
```

{{< alert title="Note" >}}The input delimiter is hard-coded to comma
(CSV) in `dfio`; output delimiters can be specified in the save
function.  This is an inherited behavior and can be changed by
following the example in the `data-frame-to-csv` function.  In
reality, most text based data we encounter are CSV, and there has not
been a need for other delimiters for input.{{< /alert >}}

### From a database

{{< alert color="warning" >}}
This functionality has been implemented, but is not
yet ready for release. It needs code cleanup, commenting, error
handling and test cases written, along with user documentation (this
section). If you need to read from a database, [open an
issue](https://github.com/Lisp-Stat/dfio/issues) and we'll get you
sorted.
{{< /alert >}}


## Saving data

Data frames can be saved into any delimited text format supported by
[cl-csv](https://github.com/AccelerationNet/cl-csv), or several
flavors of JSON, such as Vega-Lite.  Since the JSON reader/writers are
specific to the plotting applications, they are described in the
[plotting](/docs/tasks/plotting) section.

### To files

To save the `mtcars` data frame to disk, you could use:

```lisp
(data-frame-to-csv mtcars
		           :stream #P"LS:DATASETS;mtcars.csv"
                   :add-first-row t)         ; add column headers
```

and to save it to tab-separated values:

```lisp
(data-frame-to-csv mtcars
	               :separator #\tab
		           :stream #P"LS:DATASETS;mtcars.tsv"
		           :add-first-row t)         ; add column headers
```



### To a database

See the section above, [From a database](/docs/tasks/data-frame/#from-a-database).



## Summarising data

Often the first thing you'll want to do with a data frame is get a
quick summary.  You can do that with these functions.  For more
information about these functions, see the
[reference](/docs/reference/) section.

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

row-names *data-frame*
: returns a list of strings of the row names in *data-frames*

head *data-frame* &optional *n*
: displays the first *n* rows of data-frame. *n* defaults to 6.

head *data-frame* &optional *n*
: displays the last *n* rows of data-frame. *n* defaults to 6.

summary *data-frame*
: returns a summary of the variables in *data-frame*

## Manipulate columns

You have seen some of these functions before, and for completeness we
repeat them here.  The remainder of the section covers the remaining
column functions.

To obtain a variable (column) from a data frame, use the `column`
function.  Using `mtcars`, defined in [example
datasets](/docs/tasks/data-frame/#example-datasets) above:

```lisp
(column mtcars-small :mpg)
;; #(21 21 22.8d0 21.4d0 18.7d0)
```

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
(columns mtcars-small '(:mpg :wt))
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
(defparameter *d* (csv-to-data-frame
		   (format nil "Gender,Age,Height
                              \"Male\",30,180
                              \"Male\",31,182
                              \"Female\",32,165
	                          \"Male\",22,167
	                          \"Female\",45,170")))
```

```lisp
*d*
;; GENDER AGE HEIGHT 
;; Male    30    180
;; Male    31    182
;; Female  32    165
;; Male    22    167
;; Female  45    170
```

and add a 'weight' column to it:

```lisp
(add-column! *d* :weight #(75.2 88.5 49.4 78.1 79.4))
;; GENDER AGE HEIGHT WEIGHT 
;; Male    30    180   75.2
;; Male    31    182   88.5
;; Female  32    165   49.4
;; Male    22    167   78.1
;; Female  45    170   79.4
```

Now let's add multiple columns destructively using `add-columns!`

```lisp
(add-columns! *d* :a #(1 2 3 4 5) :b #(foo bar baz qux quux))
;; GENDER AGE HEIGHT WEIGHT A B
;; Male    30    180   75.2 1 FOO
;; Male    31    182   88.5 2 BAR
;; Female  32    165   49.4 3 BAZ
;; Male    22    167   78.1 4 QUX
;; Female  45    170   79.4 5 QUUX
```

And looking at `*d*`, we can see that it has been destructively
modified:

```lisp
*d*
;; GENDER AGE HEIGHT WEIGHT A B
;; Male    30    180   75.2 1 FOO
;; Male    31    182   88.5 2 BAR
;; Female  32    165   49.4 3 BAZ
;; Male    22    167   78.1 4 QUX
;; Female  45    170   79.4 5 QUUX
```

### Remove columns

Let's remove the columns `:a` and `:b` that we just added above with
the `remove-columns` function.  Since it returns a new data frame,
we'll need to assign the return value to `*d*`:

```lisp
LS-USER> (setf *d* (remove-columns *d* '(:a :b)))
;; WEIGHT HEIGHT AGE GENDER 
;;   75.2    180  30 Male
;;   88.5    182  31 Male
;;   49.4    165  32 Female
;;   78.1    167  22 Male
;;   79.4    170  45 Female
```

### Rename columns

Sometimes data sources can have variable names that we want to change.
To do this, use the `substitute-key!` function.  This example will
rename the 'gender' variable to 'sex':

```lisp
(substitute-key! *d* :sex :gender)
; => #<ORDERED-KEYS WEIGHT, HEIGHT, AGE, SEX>
```

### Replace columns

Columns are "setf-able" places and the simplest way to replace a
column is set the field to a new value:

```lisp
(df::setf (df:column *d* :gender) #("Female" "Female" "Male"))
; => #("Female" "Female" "Male")
```

Note that `df::setf` is not exported.  This is an inherited (from
Papp) behavior and likely because it bypasses checks on column
length. Use this with caution.  You can also replace a column using
two functions specifically for this purpose. Here we'll replace the
'age' column with new values:

```lisp
LS-USER> (replace-column *d* :age #(10 15 20 25 30))
;; WEIGHT HEIGHT AGE SEX
;;   75.2    180  10 Male
;;   88.5    182  15 Male
;;   49.4    165  20 Female
;;   78.1    167  25 Male
;;   79.4    170  30 Female
```

That was a non-destructive replacement, and since we didn't reassign
the value of `*d*`, it is unchanged:

```lisp
LS-USER> *d*
;; WEIGHT HEIGHT AGE SEX
;;   75.2    180  30 Male
;;   88.5    182  31 Male
;;   49.4    165  32 Female
;;   78.1    167  22 Male
;;   79.4    170  45 Female
```

We can also use the destructive version to make a permanent change
instead of `setf`-ing `*d*`:

```lisp
LS-USER> (replace-column! *d* :age #(10 15 20 25 30))
;; WEIGHT HEIGHT AGE SEX
;;   75.2    180  10 Male
;;   88.5    182  15 Male
;;   49.4    165  20 Female
;;   78.1    167  25 Male
;;   79.4    170  30 Female

LS-USER> *d*
;; WEIGHT HEIGHT AGE SEX
;;   75.2    180  10 Male
;;   88.5    182  15 Male
;;   49.4    165  20 Female
;;   78.1    167  25 Male
;;   79.4    170  30 Female
```

### Transform columns

There are two functions for column transformations.

#### replace-column
`replace-column` can also be used to transform a column by applying a
function. This example will add 20 to each value of the `:age` column:

```lisp
LS-USER> (replace-column *d* :age #'(lambda (x) (+ 20 x)))
;; WEIGHT HEIGHT AGE SEX
;;   75.2    180  30 Male
;;   88.5    182  35 Male
;;   49.4    165  40 Female
;;   78.1    167  45 Male
;;   79.4    170  50 Female
```

`replace-column!` can also apply functions to a column, destructively
modifying the column.

#### map-columns

The `map-columns` functions can be thought of as applying a function
on all the values of each variable as a vector, rather than the
individual rows as `replace-column` does.  To see this, we'll need to
use functions that operate on vectors, in this case `nu:e+`, which is
the vector addition function for Lisp-Stat.  Let's see this working
first:

```lisp
(nu:e+ #(1 1 1) #(2 3 4))
; => #(3 4 5)
```

observe how the vectors were added element-wise. We'll demonstrate
`map-columns` by adding one to each of the numeric columns in the
example data frame:

```lisp
LS-USER> (map-columns (select *d* t '(:weight :age :height)) #'(lambda (x) (nu:e+ 1 x)))
;; WEIGHT AGE HEIGHT
;;   76.2  11    181
;;   89.5  16    183
;;   50.4  21    166
;;   79.1  26    168
;;   80.4  31    171
```

recall that we used the non-destructive version of `replace-column`
above, so `*d*` has the original values. Also note the use of `select`
to get the numeric variables from the data frame; `e+` can't add
categorical values like :gender/:sex.

## Manipulate rows

### count-rows

This function is used to determine how many rows meet a certain
condition.  For example if you want to know how many cars have a MPG
(miles-per-galleon) rating greater than 20, you could use:

```lisp
(count-rows mtcars :mpg #'(lambda (x) (< 20 x)))
; => 14
```

### do-rows

`do-rows` applys a function on selected variables.  The function must
take the same number of arguments as variables supplied.  It is
analogous to [dotimes](http://clhs.lisp.se/Body/m_dotime.htm), but
iterating over data frame rows.  No values are returned; it is purely
for side-effects.  Let's create a new data data-frame to
illustrate row operations:

```lisp
LS-USER> (defparameter *d2* (make-df '(:a :b) '(#(1 2 3) #(10 20 30))))
*D2*
LS-USER> *d2*
;; A  B 
;; 1 10
;; 2 20
;; 3 30
```

This example uses `format` to illustrate iterating using `do-rows` for
side effect:

```lisp
(do-rows *d2* '(:a :b) #'(lambda (a b) (format t "~A " (+ a b))))
11 22 33
; No value
```

### map-rows

Where `map-columns` can be thought of as working through the data
frame column-by-column, `map-rows` goes through row-by-row.  Here we
add the values in each row:

```lisp
LS-USER> (map-rows *d2* '(:a :b) #'+)
#(11 22 33)
```

Since the length of this vector will always be equal to the data-frame
column length, we can add the results to the data frame as a new
column.  Let's see this in a real-world pattern, subtracting the mean
from a column:

```lisp
(add-column! *d2* :c
           (map-rows *d2* :b
                     #'(lambda (x) (- x (mean (select *d2* t :b))))))
;; A  B     C
;; 1 10 -10.0
;; 2 20   0.0
;; 3 30  10.0
```

You could also have used `replace-column!` in a similar manner to
replace a column with normalised values.


## Deleting duplicates

The `df-remove-duplicates` function will remove duplicate rows. Let's
create a data-frame with duplicates:

```lisp
(defparameter dup (make-df '(:a :b :c) '(#(a1 a1 a3)
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
LS-USER> (df-remove-duplicates dup)
;; A  B  C
;; A1 A1 A1
;; A3 B3 C3
```

<!--
## Detect missing values
-->


## Create subsets

This example assume you have saved the Rdataset mentioned above to a
variables name `mtcars`.

### mask-rows

`mask-rows` is similar to `count-rows`, except it returns a bit-vector
for rows matching the predicate.  This is useful when you want to pass
the bit vector to another function, like `select` to retrieve only the
rows matching the predicate.

```lisp
(mask-rows mtcars :mpg #'(lambda (x) (< 20 x)))
; => #*11110001100000000111100001110001
```

to make this into a filter:

```lisp
(defparameter efficient-cars
  (select mtcars (mask-rows mtcars :mpg #'(lambda (x) (< 20 x))) t)
  "Cars with MPG > 20")
```

To view them we'll need to call the `pprint` function directly instead
of using the `print-object` function we installed earlier.  Otherwise,
we'll only see the first 6.

```lisp
LS-USER> (pprint efficient-cars)

;;                 MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;; Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;; Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;; Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
;; Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
;; Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
;; Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
;; Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
;; Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
;; Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
;; Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
;; Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
;; Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
;; Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```

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

### Missing values
-->

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
