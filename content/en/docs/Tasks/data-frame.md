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
it, and then turn it into a data frame again to use in modelling or
plotting.

{{< alert title="Note" >}}In this document we refer to _column_ and
_variable_ interchangably. Likewise _factor_ and _category_ refer to a
variable type. Where neccessary we distinguish the terminology.

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
at these files with an editor like the emacs editor and you can
prepare files with your own data by following these examples.


## Creating a data-frame

A data frame can be created from a Common Lisp `array`, `alist` or
`plist`. You can also create a matrix from individual vectors of data.

Data frame columns represent sample set *variables*, and its rows
are *observations* (or cases).

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
;#<DATA-FRAME:DATA-FRAME (2 x 4)
;  :VECTOR #(1 2 3 4)
;  :SYMBOLS #(A B C D)>
```

We could also have used the `plist-df` function:

```lisp
(plist-df plist)
```

and to demonstrate the same thing using an alist, we'll use the
`alexandria:plist-alist` function to convert the `plist` into an
`alist`:

```lisp
(alist-df (plist-alist plist))
```

### From vectors
You can use `make-df` to create a data frame from keys and a list of
vectors:

```lisp
(make-df '(:a :b) '(#(1 2 3) #(10 20 30)))
```

This is useful if you've started working with variables defined with
`def`, `defparameter` or `defvar` and want to combine them into a data
frame.



## Data-frame operations

These functions operate on data-frames as a whole.  For this section of
the manual, we are going to work with this data-frame:

```lisp
(defparameter df1 (make-df '(:a :b) '(#(2 3 5) #(7 11 13))))
```

You'll use the first three of these to convert a data-frame to and
from standard lisp data structures.  This is useful if you've got data
in Common Lisp format and want to work with it in a data frame, or if
you've got a data frame and want to apply Common Lisp operators on it
that don't exist in `df`.

### as-alist

Just like it says on the tin, `as-alist` takes a data frame and
returns an `alist` version of it:

```lisp
(as-alist df1)
; => ((:A . #(2 3 5)) (:B . #(7 11 13)))
```

### as-plist

Similarly, `as-plist` will return a `plist`:

```lisp
(nu:as-plist df1)
; => (:A #(2 3 5) :B #(7 11 13))
```

Note that `as-plist` is in the `nu` (numerical-utilities) package,
where the generic `as-plist` resides.


### as-array

`as-array` returns the data frame as a row-major two dimensional lisp
array.  You'll want to save the variable names using the `keys`
function to make it easy to convert back (see
[matrix-df](#matrix-df)).  One of the reasons you might want to use
this function is to manipulate the data-frame using
[array-operations](/docs/tasks/array-operations).  This is
particularly useful when you have data frames of all numeric values.

```lisp
(aops:as-array df1)
; => #2A((2 7) (3 11) (5 13))
```

Like `as-plist`, `as-array` is in the `aops` package because the
generic array conversion functions are in the `array-operations`
system.

### columns

This function returns the variables of the data frame as a list of
vectors:

```lisp
(columns df1)
; => #(#(2 3 5) #(7 11 13))
```

This is a column-major lisp array.

You can also pass a selection to the `columns` function to return
specific columns:

```lisp
(columns df1 :a)
; => #(2 3 5)
```



### copy

`copy` returns a newly allocated data-frame with the same values as
the original:

```lisp
(copy df1)
; =>#<DATA-FRAME (2 x 3)
;     :A #(2 3 5)
;     :B #(7 11 13)>
```

Useful when applying destructive operations to the data-frame.

### keys

Returns a vector of the variables in the data frame. The keys are
symbols. Symbol properties describe the variable, for example units.

```lisp
(keys df1)
; => #(:A :B)
```


### map-df

`map-df` transforms one data-frame into another, row-by-row. Its
function signature is:

```lisp
(map-df (data-frame keys function result-keys) ...
```

It applies _function_ to each row, and returns a data frame with the
_result-keys_ as the column (variable) names.  You can also specify the
type of the new variables.

The goal for this example is to transform `df1` into a data-frame that
consists of the product of `:a` and `:b`, and a bit mask of the
columns that indicate where the value is <= 30.  First we'll need a
helper for the bit mask:

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

Note how we specified both the new key names and their type.


### matrix-df

Convert a matrix to a data-frame with the given keys.

```lisp
(matrix-df (keys df1) (aops:as-array df1))
; => #<DATA-FRAME (2 x 3)
;      :A #(2 3 5)
;      :B #(7 11 13)>
```

This is useful if you need to do a lot of numeric number-crunching on
a data set as an array, then want to add categorical variables and
continue processing as a data-frame.


### rows

`rows` returns the rows of a data frame as a list of vectors:

```lisp
(rows df1)
; => (#(2 7) #(3 11) #(5 13))
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
;#<DATA-FRAME:DATA-FRAME (3 x 3)
;  :GENDER #("Male" "Male" "Female")
;  :AGE #(30 31 32)
;  :HEIGHT #(180.0d0 182.7d0 165.0d0)>
```

`dfio` tries to hard to decipher the various number formats sometimes
encountered in CSV files:

```lisp
(select:select (dfio:csv-to-data-frame
                 (format nil "\"All kinds of wacky number formats\"~%.7~%19.~%.7f2"))
                t :all-kinds-of-wacky-number-formats)
; => #(0.7d0 19.0d0 70.0)
```



### From files

We saw above that `dfio` can read from strings, so one easy way to
read from a file is to use the `uiop` system function
`read-file-string`.  Assuming there is a file named "computers.csv" in
the current `*default-pathname-defaults*`, you could load it like
this:

```lisp
(defparameter *df* (dfio:csv-to-data-frame
		      (uiop:read-file-string "computers.csv")))
```

For most data sets, this method will work fine.  If you are working
with extremely large CSV files, you may want to consider using a
stream from an open file.


### From URLs

`dfio` can also read from Common Lisp
[streams](http://www.lispworks.com/documentation/HyperSpec/Body/21_a.htm).
Streams operations can be network or file based.  Here is an example
of how to read the same file over the network using the HTTP client
[dexador](https://github.com/fukamachi/dexador).

```lisp
(defparameter *df* (dfio:csv-to-data-frame
		      (dex:get
		       "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/computers.csv"
		       :want-stream t)))
```

{{< alert title="Note" >}}The input delimiter is hard-coded to comma
(CSV) in `dfio`; output delimiters can be specified in the save
function.  This is an inherited behaviour and can be easily changed by
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

To save our computers.csv file to disk, you could use:

```lisp
(dfio:data-frame-to-csv *df*
			  :stream #P"datasets/computers.csv"
			  :add-first-row t)         ; add column headers
```

and to save it to tab-seperated values:

```lisp
(dfio:data-frame-to-csv *df*
	          :separator #\tab
			  :stream #P"datasets/computers.csv"
			  :add-first-row t)         ; add column headers
```

### To a database

See the section above, [From a database](/docs/tasks/data-frame/#from-a-database).

## Example datasets

Vincent Arel-Bundock maintains a library of [R
datasets](https://github.com/vincentarelbundock/Rdatasets) that is a
consolidation of example data from various R packages.  The
`lisp-stat/rdata` system allows you to load these to use in Lisp-Stat.
To get started, try loading the classic `mtcars` data set:

```lisp
(ql:quickload :lisp-stat/rdata)
(rdata:load-df 'rdata::datasets 'rdata::mtcars)
;; #<DATA-FRAME:DATA-FRAME (12 x 32)
;;   :|| 1 (3%) x "Mazda RX4",
;;       1 (3%) x "Mazda RX4 Wag",
;;       1 (3%) x "Datsun 710",
;;       1 (3%) x "Hornet 4 Drive",
;;       1 (3%) x "Hornet Sportabout",
;;       1 (3%) x "Valiant",
;;       1 (3%) x "Duster 360",
;;       1 (3%) x "Merc 240D",
;;       1 (3%) x "Merc 230",
;;       1 (3%) x "Merc 280",
;;       1 (3%) x "Merc 280C",
;;       1 (3%) x "Merc 450SE",
;;       1 (3%) x "Merc 450SL",
;;       1 (3%) x "Merc 450SLC",
;;       1 (3%) x "Cadillac Fleetwood",
;;       1 (3%) x "Lincoln Continental",
;;       1 (3%) x "Chrysler Imperial",
;;       1 (3%) x "Fiat 128",
;;       1 (3%) x "Honda Civic",
;;       1 (3%) x "Toyota Corolla",
;;       1 (3%) x "Toyota Corona",
;;       1 (3%) x "Dodge Challenger",
;;       1 (3%) x "AMC Javelin",
;;       1 (3%) x "Camaro Z28",
;;       1 (3%) x "Pontiac Firebird",
;;       1 (3%) x "Fiat X1-9",
;;       1 (3%) x "Porsche 914-2",
;;       1 (3%) x "Lotus Europa",
;;       1 (3%) x "Ford Pantera L",
;;       1 (3%) x "Ferrari Dino",
;;       1 (3%) x "Maserati Bora",
;;       1 (3%) x "Volvo 142E"
;;   :MPG 32 reals, min=10.4d0, q25=15.399999698003132d0, q50=19.2d0, q75=22.8d0,
;;        max=33.9d0
;;   :CYL 14 (44%) x 8, 11 (34%) x 4, 7 (22%) x 6
;;   :DISP 32 reals, min=71.1d0, q25=120.65d0, q50=205.86666333675385d0,
;;         q75=334.0, max=472
;;   :HP 32 reals, min=52, q25=96.0, q50=123, q75=186.25, max=335
;;   :DRAT 32 reals, min=2.76d0, q25=3.08d0, q50=3.6950000000000003d0,
;;         q75=3.952000046730041d0, max=4.93d0
;;   :WT 32 reals, min=1.513d0, q25=2.5425d0, q50=3.325d0,
;;       q75=3.6766665957371387d0, max=5.424d0
;;   :QSEC 32 reals, min=14.5d0, q25=16.884999999999998d0, q50=17.71d0,
;;         q75=18.9d0, max=22.9d0
;;   :VS bits, ones: 14 (44%)
;;   :AM bits, ones: 13 (41%)
;;   :GEAR 15 (47%) x 3, 12 (38%) x 4, 5 (16%) x 5
;;   :CARB 10 (31%) x 4,
;;         10 (31%) x 2,
;;         7 (22%) x 1,
;;         3 (9%) x 3,
;;         1 (3%) x 6,
;;         1 (3%) x 8>
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
command.  Remember, this is only one package. In total there are nearly
1500 data sets in the Rdatasets library.

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


## Describe a data-frame

Often the first thing you'll want to do with a data frame is get a
quick summary.  You can do that with these functions; all take a
`data-frame` as a parameter:

`aops:nrow`
: return the number of rows in the data frame

`aops:ncol`
: return the number of columns in the data frame

`aops:dims`
: return the dimensions of the data frame

`df:keys`
: return a list of variable names

{{% pageinfo %}}**Note**: the functions below are not yet
implemented. _They would make great beginner contributions_.{{%
/pageinfo %}}

`fivenum`
: Returns Tukey's five number summary: minimum, lower-hinge, median,
  upper-hinge, maximum, for each variable in the data set (partially
  implemented)

`structure`
: Provide overview of the structure of the data frame

`variables`
: returns the name, types and units of the variables in the data frame

`head`
: displays the first 10 rows

`tail`
: displays the last 10 rows


## Manipulate columns

To obtain a variable (column) from a data frame, use the `column`
function.  Using `*d*`, defined in [Reading
data](/docs/tasks/data-frame/#reading-data) above:

```lisp
(column *d* :gender)
#("Male" "Male" "Female")
```

To get all the columns as a vector, use the `columns` function:

```lisp
(columns *d*)
#(#("Female" "Female" "Male") #(30 31 32) #(180.0d0 182.7d0 165.0d0))
```

You can also return a subset of the columns by passing in a selection:

```lisp
(columns *d* '(:gender :age))
#(#("Female" "Female" "Male") #(30 31 32))
```

### Add columns

There are two 'flavors' of add functions, destructive and
non-destructive.  The latter return a **new** data frame as the
result, and the destructive versions modify the data frame passed as a
parameter.  The destructive versions are denoted with a '!' at the end
of the function name.

To add a single column to a data frame, use the `add-column!`
function. Let's first review our data frame:

```lisp
*d*
#<DATA-FRAME (3 x 3)
  :GENDER #("Male" "Male" "Female")
  :AGE #(30 31 32)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)>
```

and add a 'weight' column to it:

```lisp
(add-column! *d* :weight #(75.2d0 88.5d0 49.4d0))
#<DATA-FRAME (4 x 3)
  :GENDER #("Male" "Male" "Female")
  :AGE #(30 31 32)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)>
```

Now let's add multiple columns destructively using `add-columns!`

```lisp
(add-columns! *d* :a #(1 2 3) :b #(foo bar baz))
#<DATA-FRAME (6 x 3)
  :GENDER #("Male" "Male" "Female")
  :AGE #(30 31 32)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :A #(1 2 3)
  :B #(FOO BAR BAZ)>
```

### Remove columns

Let's remove the columns `:a` and `:b` that we just added above with
the `remove-columns` function.  Since it returns a new data frame,
we'll need to assign the return value to `*d*`:

```lisp
(setf *d* (remove-columns *d* '(:a :b)))
#<DATA-FRAME (4 x 3)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :AGE #(30 31 32)
  :GENDER #("Male" "Male" "Female")>
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
Papp} behaviour and will probably change in future versions.  You can
also replace a column using two functions specifically for this
purpose. Here we'll replace the 'age' column with new values:

```lisp
(replace-column *d* :age #(10 15 20))
#<DATA-FRAME (4 x 3)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :AGE #(10 15 20)
  :SEX #("Male" "Male" "Female")>
```

That was a non-destructive replacement, and since we didn't reassign
the value of `*d*`, it is unchanged:

```lisp
*d*
#<DATA-FRAME (4 x 3)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :AGE #(30 31 32)
  :SEX #("Male" "Male" "Female")>
```

We can also use the destructive version to make a permanent change
instead of `setf`-ing `*d*`:

```lisp
(replace-column! *d* :age #(10 15 20))
#<DATA-FRAME (4 x 3)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :AGE #(10 15 20)
  :SEX #("Male" "Male" "Female")>
*d*
#<DATA-FRAME (4 x 3)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :AGE #(10 15 20)
  :SEX #("Male" "Male" "Female")>
```

### Transform columns

There are two functions for column transformations.

#### replace-column
`replace-column` can also be used to transform a column by applying a
function. This example will add 20 to each value of the `:age` column:

```lisp
(replace-column *d* :age #'(lambda (x) (+ 20 x)))
#<DATA-FRAME (4 x 3)
  :WEIGHT #(75.2d0 88.5d0 49.4d0)
  :HEIGHT #(180.0d0 182.7d0 165.0d0)
  :AGE #(30 35 40)
  :SEX #("Male" "Male" "Female")>
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
(map-columns (select *d* t '(:weight :age :height)) #'(lambda (x) (nu:e+ 1 x)))
#<DATA-FRAME (3 x 3)
  :WEIGHT #(76.2d0 89.5d0 50.4d0)
  :AGE #(11 16 21)
  :HEIGHT #(181.0d0 183.7d0 166.0d0)>
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
; => 18
```

### do-rows

`do-rows` applys a function on selected variables.  The function must
take the same number of arguments as variables supplied.  It is
analogous to [dotimes](http://clhs.lisp.se/Body/m_dotime.htm), but
iterating over data frame rows.  No values are returned; it is purely
for side-effects.

```lisp
(do-rows *d2* '(:a :b) #'(lambda (a b) (format t "~A " (+ a b))))
11 22 33
; No value
```

### map-rows

Where `map-columns` can be thought of as working through the data
frame column-by-column, `map-rows` goes through row-by-row. We're
going to use a different data-frame to illustrate this:

```lisp
(defparameter *d2* (make-df '(:a :b) '(#(1 2 3) #(10 20 30))))
*d2*
#<DATA-FRAME (2 x 3)
  :A #(1 2 3)
  :B #(10 20 30)>
```

Now let's add the values in each row:

```lisp
(map-rows *d2* '(:a :b) #'+)
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
#<DATA-FRAME (3 x 3)
  :A #(1 2 3)
  :B #(10 20 30)
  :C #(-10 0 10)>
```

You could also have used `replace-column!` in a similar manner to
replace a column with normalised values.


## Deleting duplicates

The `df-remove-duplicates` function will remove duplicate rows. Let's
create a data-frame with duplicates:

```lisp
(defparameter dup (make-df '(:a :b :c) '(#(a a 3) #(a a 3) #(a a 333))))
;#<DATA-FRAME (3 x 3)
;  :A #(A A 3)
;  :B #(A A 3)
;  :C #(A A 333)>
```

Now remove duplicate rows of 'A':

```lisp
(df-remove-duplicates dup)
;#<DATA-FRAME (3 x 2)
;  :A #(A 3)
;  :B #(A 3)
;  :C #(A 333)>
```

<!--
## Detect missing values
-->


## Create subsets

This example assume you have saved the Rdataset mentioned above to a
variables name `mtcars`.

### mask-rows

`mask-rows` is similar to `count-rows`, except it return a bit-vector
for rows matching the predicate.  This is useful when you want to pass
the bit vector to another function, like `select` to retrieve only the
rows matching the predicate.

```lisp
(mask-rows mtcars :mpg #'(lambda (x) (< 20 x)))
; => #*11110001100000000111100001110001
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
- parse timestrings,
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
