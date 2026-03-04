---
title: "Data Frame"
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
[array-operations](/docs/manuals/array-operations) to manipulate it, and
then turn it into a data frame again to use in modeling or plotting.

Data frame is implemented as a two-dimensional common lisp data
structure: a vector of vectors for data, and a hash table mapping
variable names to column vectors.  All columns are of equal length.
This structure provides the flexibility required for column oriented
manipulation, as well as speed for large data sets.

{{< alert title="Note" >}}In this document we refer to _column_ and
_variable_ interchangeably. Likewise _factor_ and _category_ refer to a
variable type. Where necessary we distinguish the terminology.{{</alert >}}

### Load/install

Data-frame is part of the Lisp-Stat package. It can be used
independently if desired. Since the examples in this manual use
Lisp-Stat functionality, we'll use it from there rather than load
independently.

```lisp
(ql:quickload :lisp-stat)
```

Within the Lisp-Stat system, the `LS-USER` package is the package for
you to do statistics work. Type the following to change to that
package:

```lisp
(in-package :ls-user)
```

{{< alert title="Note" >}}The examples assume that you are in package
LS-USER. You should make a habit of always working from the `LS-USER`
package.  All the samples may be copied to the clipboard using the
`copy` button in the upper-right corner of the sample code
box.{{</alert >}}

<!-- TODO Figure out whether we want to keep data variables
## Data variables

If you're collecting data and exploring a problem domain, you'll
sometimes have a collection of separate variable to start with.
Common Lisp has two structures for holding multiple observations of
variables: _list_ and _vector_, collectively known as a _sequence_.
For the most part a _vector_ is more efficient, and the recommended
way to work with variables that are independent of a data-frame.

### defparameter
Lisp-Stat provides a wrapper over Common Lisp's `defparameter`
function to make working with data variables a little easier.  This
macro puts the parameter on a global list, and provides a way of
removing it from the environment when required.

You define a data variable with the `def` function. Here are some
variables containing weather data in Singapore over the last 14 days:

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

To save a variable you can use the `savevar` function.  This function
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
-->

### Naming conventions

Lisp-Stat has a few naming conventions you should be aware of.  If you
see a punctuation mark or the letter 'p' as the last letter of a
function name, it indicates something about the function:

- '!' indicates that the function is _destructive_.  It will modify the data that you pass to it.  Otherwise, it will return a copy that you will need to save in a variable.
- 'p', '-p' or '?' means the function is a _predicate_, that returns a Boolean truth value.


### Data frame environment
Although you can work with data frames bound to symbols (as would
happen if you used `(defparameter ...)`, it is more convenient to
define them as part of an environment.  When you do this, the system
defines a package of the same name as the data frame, and provides a
symbol for each variable.  Let's see how things work _without_ an
environment:

First, we define a data frame as a parameter:

```lisp
(defparameter mtcars (read-csv rdata:mtcars)
"Motor Trend Car Road Tests")
;; WARNING: Missing column name was filled in
;; MTCARS2
```

Now if we want a column, we can say:

```lisp
(column mtcars 'mpg)
```

Now let's define an environment using `defdf`:

```lisp
(defdf mtcars (read-csv rdata:mtcars)
"Motor Trend Car Road Tests")
;; WARNING: Missing column name was filled in
;; #<DATA-FRAME (32 observations of 12 variables)
;; Motor Trend Car Road Tests>
```

Now we can access the same variable with:

```lisp
mtcars:mpg
```

`defdf` does a lot more than this, and you should probably use `defdf` to set up an environment instead of `defparameter`.  We mention it here because there's an important bit about maintaining the environment to be aware of:

{{< alert title="Note" >}}_Destructive_ functions (those ending in '!'), will automatically update the environment for you.  Functions that return a _copy_ of the data will not.{{</alert >}}

### defdf

The `defdf` macro is conceptually equivalent to the Common
Lisp `defparameter`, but with some additional functionality that makes
working with data frames easier.  You use it the same way you'd use
`defparameter`, for example:

```lisp
(defdf foo <any-function returning a data frame> )
```

We'll use both ways of defining data frames in this manual.  The access
methods that are defined by `defdf` are described in the
[access data](#access-data) section.

## Data types

It is important to note that there are two 'types' in Lisp-Stat: the
implementation type and the 'statistical' type.  Sometimes these are
the same, such as in the case of `reals`; in other situations they are
not.  A good example of this can be seen in the `mtcars` data set.  The
`hp` (horsepower), `gear` and `carb` are all of type `integer` from an
implementation perspective.  However only `horsepower` is a continuous
variable.  You *can* have an additional 0.5 horsepower, but you cannot
add an additional 0.5 gears or carburetors.

Data types are one kind of *property* that can be set on a variable.

As part of the recoding and data cleansing process, you will want to add
properties to your variables.  In Common Lisp, these are `plists` that
reside on the variable symbols, e.g. `mtcars:mpg`.  In R they are
known as `attributes`.  By default, there are three properties for
each variable: type, unit and label (documentation).  When you load
from external formats, like CSV, these properties are all `nil`; when
you load from a lisp file, they will have been saved along with the
data (if you set them).

There are seven data types in Lisp-Stat:

- string
- integer
- double-float
- single-float
- categorical (`factor` in R)
- temporal
- bit (Boolean)

### Numeric

Numeric types, `double-float`, `single-float` and `integer` are all
essentially similar.  The vector versions have type definitions (from
the numeric-utilities package) of:

- simple-double-float-vector
- simple-single-float-vector
- simple-fixnum-vector

As an example, let's look at `mtcars:mpg`, where we have a variable of
type float, but a few integer values mixed in.

The *values* may be equivalent, but the *types* are not.  The CSV
loader has no way of knowing, so loads the column as a mixture of
integers and floats.  Let's start by reloading `mtcars` from the CSV
file:

```lisp
(undef 'mtcars)
(defdf mtcars (read-csv rdata:mtcars))
```

and look at the `mpg` variable:

```lisp
LS-USER> mtcars:mpg
#(21 21 22.8d0 21.4d0 18.7d0 18.1d0 14.3d0 24.4d0 22.8d0 19.2d0 17.8d0 16.4d0
  17.3d0 15.2d0 10.4d0 10.4d0 14.7d0 32.4d0 30.4d0 33.9d0 21.5d0 15.5d0 15.2d0
  13.3d0 19.2d0 27.3d0 26 30.4d0 15.8d0 19.7d0 15 21.4d0)
LS-USER> (type-of *)
(SIMPLE-VECTOR 32)
```

Notice that the first two entries in the vector are integers, and the
remainder floats. To fix this manually, you will need to coerce each
element of the column to type `double-float` (you could use
`single-float` in this case; as a matter of habit we usually use
`double-float`) and then change the type of the vector to a
specialised `float` vector.

You can use the `heuristicate-types` function to guess the statistical
types for you.  For `reals` and `strings`, `heuristicate-types` works
fine, however because `integers` and `bits` can be used to encode
`categorical` or numeric values, you will have to indicate the type
using `set-properties`.  We see this below with `gear` and `carb`,
although implemented as `integer`, they are actually type
`categorical`.  The next sections describes how to set them.

Using [describe](/docs/manuals/data-frame/#describe), we can view the
types of all the variables that `heuristicate-types` set:

```lisp
LS-USER> (heuristicate-types mtcars)
LS-USER> (describe mtcars)
MTCARS
  A data-frame with 32 observations of 12 variables

Variable | Type         | Unit | Label
-------- | ----         | ---- | -----------
X8       | STRING       | NIL  | NIL
MPG      | DOUBLE-FLOAT | NIL  | NIL
CYL      | INTEGER      | NIL  | NIL
DISP     | DOUBLE-FLOAT | NIL  | NIL
HP       | INTEGER      | NIL  | NIL
DRAT     | DOUBLE-FLOAT | NIL  | NIL
WT       | DOUBLE-FLOAT | NIL  | NIL
QSEC     | DOUBLE-FLOAT | NIL  | NIL
VS       | BIT          | NIL  | NIL
AM       | BIT          | NIL  | NIL
GEAR     | INTEGER      | NIL  | NIL
CARB     | INTEGER      | NIL  | NIL
```

Notice the system correctly typed `vs` and `am` as Boolean (`bit`)
(correct in a mathematical sense)

### Strings

Unlike in R, strings are *not* considered categorical variables by
default.  Ordering of strings varies according to locale, so it's not a
good idea to rely on the strings.  Nevertheless, they do work well if
you are working in a single locale.

### Categorical

Categorical variables have a fixed and known set of possible values.
In `mtcars`, `gear`, `carb` `vs` and `am` are categorical variables,
but `heuristicate-types` can't distinguish `categorical` types, so
we'll set them:

```lisp
(set-properties mtcars :type '(:vs :categorical
			                    :am :categorical
			                    :gear :categorical
			                    :carb :categorical))
```

### Temporal

Dates and times can be surprisingly complicated.  To make working with
them simpler, Lisp-Stat uses vectors of
[localtime](https://github.com/dlowe-net/local-time) objects to
represent dates & times.  You can set a temporal type with
`set-properties` as well using the keyword `:temporal`.


### Units & labels

To add units or labels to the data frame, use the `set-properties`
function.  This function takes a plist of variable/value pairs, so to
set the units and labels:

```lisp
(set-properties mtcars :unit '(:mpg m/g
	                   :cyl :NA
			           :disp in³
			           :hp hp
			           :drat :NA
			           :wt lb
			           :qsec s
			           :vs :NA
			           :am :NA
			           :gear :NA
			           :carb :NA))

(set-properties mtcars :label '(:mpg "Miles/(US) gallon"
				       :cyl "Number of cylinders"
                       :disp "Displacement (cu.in.)"
				       :hp "Gross horsepower"
				       :drat "Rear axle ratio"
				       :wt "Weight (1000 lbs)"
				       :qsec "1/4 mile time"
				       :vs "Engine (0=v-shaped, 1=straight)"
				       :am "Transmission (0=automatic, 1=manual)"
				       :gear "Number of forward gears"
				       :carb "Number of carburetors"))
```

Now look at the description again:

```lisp
LS-USER> (describe mtcars)
MTCARS
  A data-frame with 32 observations of 12 variables

Variable | Type         | Unit | Label
-------- | ----         | ---- | -----------
X8       | STRING       | NIL  | NIL
MPG      | DOUBLE-FLOAT | M/G  | Miles/(US) gallon
CYL      | INTEGER      | NA   | Number of cylinders
DISP     | DOUBLE-FLOAT | IN3  | Displacement (cu.in.)
HP       | INTEGER      | HP   | Gross horsepower
DRAT     | DOUBLE-FLOAT | NA   | Rear axle ratio
WT       | DOUBLE-FLOAT | LB   | Weight (1000 lbs)
QSEC     | DOUBLE-FLOAT | S    | 1/4 mile time
VS       | BIT          | NA   | Engine (0=v-shaped, 1=straight)
AM       | BIT          | NA   | Transmission (0=automatic, 1=manual)
GEAR     | INTEGER      | NA   | Number of forward gears
CARB     | INTEGER      | NA   | Number of carburetors
```

You can set your own properties with this command too.  To make your
custom properties appear in the `describe` command and be saved
automatically, override the `describe` and `write-df` methods, or use
`:after` methods.


## Create data-frames

A data frame can be created from a Common Lisp `array`, `alist`,
`plist`, individual data vectors, another data frame or a vector-of
vectors.  In this section we'll describe creating a data frame from each of these.

Data frame columns represent sample set *variables*, and its rows
are *observations* (or cases).

{{< alert title="Note" >}}
For these examples we are going to install a modified version of the
Lisp-Stat data-frame print-object function. This will cause the REPL
to display the data-frame at creation, and save us from having to type
(pprint data-frame) in each example.  If you'd like to install it as we
have, execute the code below at the REPL.
{{< /alert >}}


```lisp
(defmethod print-object ((df data-frame) stream)
  "Print the first six rows of DATA-FRAME"
  (let ((*print-lines* 6))
    (pprint df stream nil)))

(set-pprint-dispatch 'df:data-frame
		     #'(lambda (s df) (pprint df s nil)))
```

You can ignore the warning that you'll receive after executing the
code above.

Let's create a simple data frame. First we'll setup some
variables (columns) to represent our sample domain:

```lisp
(defparameter v #(1 2 3 4)) ; vector
(defparameter b #*0110)     ; bits
(defparameter s #(a b c d)) ; symbols
(defparameter plist `(:vector ,v :symbols ,s)) ;only v & s
```

Let's print `plist`.  Just type the name in at the REPL prompt.

```lisp
plist
(:VECTOR #(1 2 3 4) :SYMBOLS #(A B C D))
```

### From p/a-lists

Now suppose we want to create a data frame from a `plist`

```lisp
(apply #'df plist)

;; VECTOR SYMBOLS
;;      1       A
;;      2       B
;;      3       C
;;      4       D

```

We could also have used the `plist-df` function:

```lisp
(plist-df plist)

;; VECTOR SYMBOLS
;;      1       A
;;      2       B
;;      3       C
;;      4       D
```

and to demonstrate the same thing using an alist, we'll use the
`alexandria:plist-alist` function to convert the `plist` into an
`alist`:

```lisp
(alist-df (plist-alist plist))

;; VECTOR SYMBOLS
;;      1       A
;;      2       B
;;      3       C
;;      4       D
```

### From vectors
You can use `make-df` to create a data frame from keys and a list of
vectors. Each vector becomes a column in the data-frame.

```lisp
(make-df '(:a :b)                 ; the keys
         '(#(1 2 3) #(10 20 30))) ; the columns
;; A  B
;; 1 10
;; 2 20
;; 3 30
```

This is useful if you've started working with variables defined with
`defparameter` or `defvar` and want to combine them into a data frame.

### From arrays

`matrix-df` converts a matrix (array) to a data-frame with the given
keys.

```lisp
(matrix-df #(:a :b) #2A((1 2)
	                    (3 4)))
;#<DATA-FRAME (2 observations of 2 variables)>
```

This is useful if you need to do a lot of numeric number-crunching on
a data set as an array, perhaps with BLAS or `array-operations` then
want to add categorical variables and continue processing as a
data-frame.


### Example datasets

Vincent Arel-Bundock maintains a library of over 1700 [R
datasets](https://github.com/vincentarelbundock/Rdatasets) that is a
consolidation of example data from various R packages. You can load
one of these by specifying the url to the `raw` data to the `read-csv`
function. For example to load the
[iris](https://github.com/vincentarelbundock/Rdatasets/blob/master/csv/datasets/iris.csv)
data set, use:

```lisp
(defdf iris
	(read-csv "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/iris.csv")
	"Edgar Anderson's Iris Data")
```

#### Default datasets

To make the examples and tutorials easier, Lisp-Stat includes the URLs
for the R built in data sets.  You can see these by viewing the
`rdata:*r-default-datasets*` variable:

```lisp
LS-USER? rdata:*r-default-datasets*
(RDATA:AIRPASSENGERS RDATA:ABILITY.COV RDATA:AIRMILES RDATA:AIRQUALITY
 RDATA:ANSCOMBE RDATA:ATTENU RDATA:ATTITUDE RDATA:AUSTRES RDATA:BJSALES
 RDATA:BOD RDATA:CARS RDATA:CHICKWEIGHT RDATA:CHICKWTS RDATA:CO2-1 RDATA:CO2-2
 RDATA:CRIMTAB RDATA:DISCOVERIES RDATA:DNASE RDATA:ESOPH RDATA:EURO
 RDATA:EUSTOCKMARKETS RDATA:FAITHFUL RDATA:FORMALDEHYDE RDATA:FREENY
 RDATA:HAIREYECOLOR RDATA:HARMAN23.COR RDATA:HARMAN74.COR RDATA:INDOMETH
 RDATA:INFERT RDATA:INSECTSPRAYS RDATA:IRIS RDATA:IRIS3 RDATA:ISLANDS
 RDATA:JOHNSONJOHNSON RDATA:LAKEHURON RDATA:LH RDATA:LIFECYCLESAVINGS
 RDATA:LOBLOLLY RDATA:LONGLEY RDATA:LYNX RDATA:MORLEY RDATA:MTCARS RDATA:NHTEMP
 RDATA:NILE RDATA:NOTTEM RDATA:NPK RDATA:OCCUPATIONALSTATUS RDATA:ORANGE
 RDATA:ORCHARDSPRAYS RDATA:PLANTGROWTH RDATA:PRECIP RDATA:PRESIDENTS
 RDATA:PRESSURE RDATA:PUROMYCIN RDATA:QUAKES RDATA:RANDU RDATA:RIVERS
 RDATA:ROCK RDATA:SEATBELTS RDATA::STUDENT-SLEEP RDATA:STACKLOSS
 RDATA:SUNSPOT.MONTH RDATA:SUNSPOT.YEAR RDATA:SUNSPOTS RDATA:SWISS RDATA:THEOPH
 RDATA:TITANIC RDATA:TOOTHGROWTH RDATA:TREERING RDATA:TREES RDATA:UCBADMISSIONS
 RDATA:UKDRIVERDEATHS RDATA:UKGAS RDATA:USACCDEATHS RDATA:USARRESTS
 RDATA:USJUDGERATINGS RDATA:USPERSONALEXPENDITURE RDATA:USPOP RDATA:VADEATHS
 RDATA:VOLCANO RDATA:WARPBREAKS RDATA:WOMEN RDATA:WORLDPHONES RDATA:WWWUSAGE)
```

To load one of these, you can use the name of the data set. For example to load `mtcars`:

```lisp
(defdf mtcars
  (read-csv rdata:mtcars))
```

If you want to load all of the default R data sets, use the
`rdata:load-r-default-datasets` command. All the data sets included in
base R will now be loaded into your environment. This is useful if you
are following a R tutorial, but using Lisp-Stat for the analysis
software.

You may also want to save the default R data sets in order to augment
the data with labels, units, types, etc. To save all of the default R
data sets to the `LS:DATA;R` directory, use the
`(rdata:save-r-default-datasets)` command if the default data sets
have already been loaded, or `save-r-data` if they have not. This
saves the data in lisp format.

#### Install R datasets

To work with all of the R data sets, we recommend you use `git` to
download the repository to your hard drive. For example I downloaded the
example data to the `s:` drive like this:

```sh
cd s:
git clone https://github.com/vincentarelbundock/Rdatasets.git
```

and setup a logical host in my `ls-init.lisp` file like so:

```lisp
;;; Define logical hosts for external data sets
(setf (logical-pathname-translations "RDATA")
	`(("**;*.*.*" ,(merge-pathnames "csv/**/*.*" "s:/Rdatasets/"))))
```

Now you can access any of the datasets using the logical
pathname. Here's an example of creating a data frame using the
`ggplot` `mpg` data set:

```lisp
(defdf mpg (read-csv #P"RDATA:ggplot2;mpg.csv"))
```

#### Searching the examples

With so many data sets, it's helpful to load the index into a data
frame so you can search for specific examples. You can do this by
loading the `rdata:index` into a data frame:

```lisp
(defdf rindex (read-csv rdata:index))
```

I find it easiest to use the [SQL-DF](/docs/manuals/sqldf) system
to query this data. For example if you wanted to find the data sets
with the largest number of observations:

```lisp
(ql:quickload :sqldf)
(pprint
	(sqldf:sqldf "select item, title, rows, cols from rindex order by rows desc limit 10"))

;;   ITEM            TITLE                                                               ROWS COLS
;; 0 military        US Military Demographics                                         1414593    6
;; 1 Birthdays       US Births in 1969 - 1988                                          372864    7
;; 2 wvs_justifbribe Attitudes about the Justifiability of Bribe-Taking in the ...     348532    6
;; 3 flights         Flights data                                                      336776   19
;; 4 wvs_immig       Attitudes about Immigration in the World Values Survey            310388    6
;; 5 Fertility       Fertility and Women's Labor Supply                                254654    8
;; 6 avandia         Cardiovascular problems for two types of Diabetes medicines       227571    2
;; 7 AthleteGrad     Athletic Participation, Race, and Graduation                      214555    3
;; 8 mortgages       Data from "How do Mortgage Subsidies Affect Home Ownership? ..."  214144    6
;; 9 mammogram       Experiment with Mammogram Randomized
```


## Export data frames

These next few functions are the reverse of the ones above used to
create them. These are useful when you want to use foreign libraries
or common lisp functions to process the data.

For this section of the manual, we are going to work with a subset of
the `mtcars` data set from above. We'll use the
[select](/docs/manuals/select/) package to take the first 5 rows so that
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
welcome addition to Lisp-Stat)

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
(as-plist mtcars-small)
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
[keys](/docs/manuals/data-frame/#keys) function to make it easy to
convert back (see [matrix-df](#from-arrays)).  One of the reasons you
might want to use this function is to manipulate the data-frame using
[array-operations](/docs/manuals/array-operations).  This is
particularly useful when you have data frames of all numeric values.

```lisp
(defparameter mtcars-keys (keys mtcars)) ; we'll use later
(defparameter mtcars-small-array (as-array mtcars-small))
mtcars-small-array
;; 0 Mazda RX4         21.0 6 160 110 3.90 2.620 16.46 0 1 4 4
;; 1 Mazda RX4 Wag     21.0 6 160 110 3.90 2.875 17.02 0 1 4 4
;; 2 Datsun 710        22.8 4 108  93 3.85 2.320 18.61 1 1 4 1
;; 3 Hornet 4 Drive    21.4 6 258 110 3.08 3.215 19.44 1 0 3 1
;; 4 Hornet Sportabout 18.7 8 360 175 3.15 3.440 17.02 0 0 3 2
```

Our abbreviated `mtcars` data frame is now a two dimensional Common
Lisp array. It may not look like one because Lisp-Stat will 'print
pretty' arrays. You can inspect it with the `describe` command to make
sure:

```lisp
LS-USER> (describe mtcars-small-array)
...

Type: (SIMPLE-ARRAY T (5 12))
Class: #<BUILT-IN-CLASS SIMPLE-ARRAY>
Element type: T
Rank: 2
Physical size: 60
```

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
(columns mtcars-small 'mpg)
; #(21 21 22.8d0 21.4d0 18.7d0)
```

The functions in [array-operations](/docs/manuals/array-operations/) are
helpful in further dealing with data frames as vectors and arrays. For
example you could convert a data frame to a transposed array by using
[aops:combine](/docs/manuals/array-operations#combine) with the
`columns` function:

```lisp
(combine (columns mtcars-small))
;;  0 Mazda RX4 Mazda RX4 Wag Datsun 710 Hornet 4 Drive Hornet Sportabout
;;  1     21.00        21.000      22.80         21.400             18.70
;;  2      6.00         6.000       4.00          6.000              8.00
;;  3    160.00       160.000     108.00        258.000            360.00
;;  4    110.00       110.000      93.00        110.000            175.00
;;  5      3.90         3.900       3.85          3.080              3.15
;;  6      2.62         2.875       2.32          3.215              3.44
;;  7     16.46        17.020      18.61         19.440             17.02
;;  8      0.00         0.000       1.00          1.000              0.00
;;  9      1.00         1.000       1.00          0.000              0.00
;; 10      4.00         4.000       4.00          3.000              3.00
;; 11      4.00         4.000       1.00          1.000              2.00
```

### json

The [json-to-data-frame](https://github.com/gassechen/json-to-data-frame) by 'gassechen' can be used to convert JSON to a `data-frame`. The following example is taken from that repo.  Note that the system is not in Quicklisp, so you'll have to obtain it manually (see instructions in the repo).

1. Define the URL for the JSON API

   ``` commonlisp
   (defparameter *url* "https://jsonplaceholder.typicode.com/posts")
   ```

2. Define a function to call the API and parse the JSON response

   ``` commonlisp
   (defun call-api (url-get)
     (let* ((yason:*parse-json-booleans-as-symbols* t)
            (yason:*parse-json-arrays-as-vectors* nil)
            (respuesta
              (yason:parse
               (dex:get url-get
                        :keep-alive t
                        :use-connection-pool t
                        :connect-timeout 60
                        :want-stream t))))
       respuesta))
   ```

3. Convert the JSON response to a data frame

   ``` commonlisp
   (json-to-df (call-api *url*))
   ```

   You will be prompted to select a symbol to be made accessible in the
   DFIO package:

   ``` example
   Select a symbol to be made accessible in package DFIO:
     1. DATA-FRAME::BODY
     2. DFIO::BODY

   Enter an integer (between 1 and 2): 1
   ```

4. Display the data frames

   ``` commonlisp
   (lisp-stat:show-data-frames)
   ```

5. Assign the data frame to a variable and print it

   ``` commonlisp
   (json-to-df (call-api *url*) "my-df")
   (lisp-stat:show-data-frames)
   ```

6. Print the data frame

   ``` commonlisp
   (pprint my-df)
   ```


## Load data

There are two functions for loading data.  The first `data` makes
loading from logical pathnames convenient.  The other, `read-csv`
works with the file system or URLs. Although the name `read-csv`
implies only CSV (comma separated values), it can actually read with
other delimiters, such as the tab character.  See the [DFIO API
reference](/docs/reference/dfio/) for more information.

### The data command

For built in Lisp-Stat data sets, you can load with just the data set
name.  For example to load `mtcars`:

```lisp
(data :mtcars)
```

If you've [installed the R data
sets](/docs/manuals/data-frame/#example-datasets), and want to load
the `antigua` data set from the `daag` package, you could do it like
this:

```lisp
(data :antigua :system :rdata :directory :daag :type :csv)
```

If the file type is not `lisp` (say it's TSV or CSV), you need to
specify the `type` parameter.


### From strings

Here is a short demonstration of reading from strings:

```lisp
(defparameter *d*
  (read-csv
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

### From delimited files

We saw above that `dfio` can read from strings, so one easy way to
read from a file is to use the `uiop` system function
`read-file-string`.  We can read one of the example data files
included with Lisp-Stat like this:

```lisp
(read-csv
	(uiop:read-file-string #P"LS:DATA;absorbtion.csv"))
;;    IRON ALUMINUM ABSORPTION 
;;  0   61       13          4
;;  1  175       21         18
;;  2  111       24         14
;;  3  124       23         18
;;  4  130       64         26
;;  5  173       38         26 ..
```

That example just illustrates reading from a file to a string.  In
practice you're better off just reading the file in directly and avoid
reading into a string first:

```lisp
(read-csv #P"LS:DATA;absorbtion.csv")
;;    IRON ALUMINUM ABSORPTION
;;  0   61       13          4
;;  1  175       21         18
;;  2  111       24         14
;;  3  124       23         18
;;  4  130       64         26
;;  5  173       38         26 ..
```

### From parquet files

You can use the [duckdb](https://github.com/ak-coram/cl-duckdb) system to load data from parquet files:

```
(ql:quickload :duckdb) ; see duckdb repo for installation instructions
(ddb:query "INSTALL httpfs;" nil) ; loading via http
(ddb:initialize-default-connection)
(defdf yellow-taxis
    (let ((q (ddb:query "SELECT * FROM read_parquet('https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2023-01.parquet') LIMIT 10" nil)))
      (make-df (mapcar #'dfio:string-to-symbol (alist-keys q))
	       (alist-values q))))
```
Now we can find the average fare:
```
(mean yellow-taxis:fare-amount)
11.120000000000001d0
```


### From URLs

`dfio` can also read from Common Lisp
[streams](http://www.lispworks.com/documentation/HyperSpec/Body/21_a.htm).
Stream operations can be network or file based.  Here is an example
of how to read the classic Iris data set over the network:

```lisp
(read-csv
   "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/iris.csv")

;;     X27 SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH SPECIES
;;   0   1          5.1         3.5          1.4         0.2 setosa
;;   1   2          4.9         3.0          1.4         0.2 setosa
;;   2   3          4.7         3.2          1.3         0.2 setosa
;;   3   4          4.6         3.1          1.5         0.2 setosa
;;   4   5          5.0         3.6          1.4         0.2 setosa
;;   5   6          5.4         3.9          1.7         0.4 setosa ..
```


### From a database

You can load data from a SQLite table using the
[read-table](/docs/manuals/sqldf#read-a-data-frame)
command. Here's an example of reading the `iris` data frame from a
SQLite table:

```lisp
(asdf:load-system :sqldf)
(defdf iris
	(sqldf:read-table
		(sqlite:connect #P"S:\\src\\lisp-stat\\data\\iris.db3")
		"iris"))
```

Note that `sqlite:connect` does not take a logical pathname; use a
system path appropriate for your computer. One reason you might want
to do this is for speed in loading CSV. The CSV loader for SQLite is
10-15 times faster than the fastest Common Lisp CSV parser, and it is
often quicker to load to SQLite first, then load into Lisp.

## Save data

Data frames can be saved into any delimited text format supported by
[fare-csv](https://github.com/fare/fare-csv), or several
flavors of JSON, such as Vega-Lite.

<!-- Since the JSON reader/writers are
specific to the plotting applications, they are described in the
[plotting](/docs/manuals/plot) section.-->

### As CSV

To save the `mtcars` data frame to disk, you could use:

```lisp
(write-csv mtcars
		   #P"LS:DATA;mtcars.csv"
           :add-first-row t)         ; add column headers
```

to save it as CSV, or to save it to tab-separated values:

```lisp
(write-csv mtcars
	       #P"LS:DATA;mtcars.tsv"
	       :separator #\tab
		   :add-first-row t)         ; add column headers
```

### As Lisp

For the most part, you will want to save your data frames as
lisp. Doing so is both faster in loading, but more importantly it
preserves any variable attributes that may have been given.

To save a data frame, use the `save` command:

```lisp
(save 'mtcars #P"LS:DATA;mtcars-example")
```

Note that in this case you are passing the *symbol* to the function,
not the value (thus the quote (') before the name of the data frame).
Also note that the system will add the 'lisp' suffix for you.


### To a database

The [write-table](/docs/manuals/sqldf#write-a-data-frame) function
can be used to save a data frame to a SQLite database. Each take a
connection to a database, which may be file or memory based, a table
name and a data frame. Multiple data frames, with different table
names, may be written to a single SQLite file this way.


## Access data

This section describes various way to access data variables.


### Define a data-frame

Let's use `defdf` to define the `iris` data
frame. We'll use both of these data frames in the examples below.

```lisp
(defdf iris
  (read-csv rdata:iris))
;WARNING: Missing column name was filled in
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

Notice a couple of things.  First, there is a column `X29`. In fact if
you look back at previous data frame output in this tutorial you will
notice various columns named `X` followed by some number.  This is
because the column was not given a name in the data set, so a name was
generated for it. `X` starts at 1 and increased by 1 each time an
unnamed variable is encountered during your Lisp-Stat session.  The
next time you start Lisp-Stat, numbering will begin from 1 again.
We will see how to clean this up this data frame in the next sections.

The second thing to note is the row numbers on the far left side.
When Lisp-Stat prints a data frame it automatically adds row
numbers. Row and column numbering in Lisp-Stat start at 0.  In R they
start with 1.  Row numbers make it convenient to select data sections
from a data frame, but they are not part of the data and cannot be
selected or manipulated themselves.  They only appear when a data
frame is printed.

### Access a variable

The `defdf` macro also defines symbol macros that allow you to refer
to a variable by name, for example to refer to the `mpg` column of
mtcars, you can refer to it by the the name `data-frame:variable`
convention.


```lisp
mtcars:mpg
; #(21 21 22.8D0 21.4D0 18.7D0 18.1D0 14.3D0 24.4D0 22.8D0 19.2D0 17.8D0 16.4D0
  17.3D0 15.2D0 10.4D0 10.4D0 14.7D0 32.4D0 30.4D0 33.9D0 21.5D0 15.5D0 15.2D0
  13.3D0 19.2D0 27.3D0 26 30.4D0 15.8D0 19.7D0 15 21.4D0)
```

There is a point of distinction to be made here: the _values_ of `mpg`
and the _column_ `mpg`. For example to obtain the same vector using
the selection/sub-setting package `select` we must refer to the
_column_:

```lisp
(select mtcars t 'mpg)
; #(21 21 22.8D0 21.4D0 18.7D0 18.1D0 14.3D0 24.4D0 22.8D0 19.2D0 17.8D0 16.4D0
  17.3D0 15.2D0 10.4D0 10.4D0 14.7D0 32.4D0 30.4D0 33.9D0 21.5D0 15.5D0 15.2D0
  13.3D0 19.2D0 27.3D0 26 30.4D0 15.8D0 19.7D0 15 21.4D0)
```

Note that with `select` we passed the _symbol_ `'mpg` (you can
tell it's a symbol because of the quote in front of it).

So, the rule here is: if you want the _value_ refer to it directly,
e.g. `mtcars:mpg`. If you are referring to the _column_, use the
symbol.  Data frame operations sometimes require the symbol, where as
Common Lisp and other packages that take vectors use the direct access
form.


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
; #(X45 MPG CYL DISP HP DRAT WT QSEC VS AM GEAR CARB)
```

Recall the earlier discussion of `X1` for the column name.


### map-df

`map-df` transforms one data-frame into another, row-by-row. Its
function signature is:

```
(map-df data-frame keys function result-keys) ...
```

It applies _function_ to each row, and returns a data frame with the
_result-keys_ as the column (variable) names.  `keys` is a _list_.
You can also specify the type of the new variables in the
`result-keys` list.

The goal for this example is to transform `df1`:

```lisp
(defparameter df1 (make-df '(:a :b) '(#(2  3  5)
                                      #(7 11 13))))
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
(defparameter df2 (map-df df1
                          '(:a :b)
			              (lambda (a b)
			                (vector (* a b) (predicate-bit a b)))
			              '((:p fixnum) (:m bit))))
```
Since it was a parameter assignment, we have to view it manually:

```lisp
(print-df df2)
;;    P M
;; 0 14 0
;; 1 33 1
;; 2 65 1
```

Note how we specified both the new key names and their type.  Here's
an example that transforms the units of `mtcars` from imperial to metric:

```lisp
(map-df mtcars '(x1 mpg disp hp wt)
	(lambda (model mpg disp hp wt)
	  (vector model ;no transformation for model (X1), return as-is
              (/ 235.214583 mpg)
		      (/ disp 61.024)
		      (* hp 1.01387)
		      (/ (* wt 1000) 2.2046)))
	'(:model (:100km/l float) (:disp float) (:hp float) (:kg float)))

;;    MODEL                 100KM/L    DISP        HP         KG
;;  0 Mazda RX4             11.2007  2.6219  111.5257  1188.4242
;;  1 Mazda RX4 Wag         11.2007  2.6219  111.5257  1304.0914
;;  2 Datsun 710            10.3164  1.7698   94.2899  1052.3451
;;  3 Hornet 4 Drive        10.9913  4.2278  111.5257  1458.3144
;;  4 Hornet Sportabout     12.5783  5.8993  177.4272  1560.3737
;;  5 Valiant               12.9953  3.6871  106.4564  1569.4456 ..
```

Note that you may have to adjust the `X` column name to suit your
current environment.

You might be wondering how we were able to refer to the columns
without the ' (quote); in fact we did, at the beginning of the
list. The lisp reader then reads the contents of the list as symbols.

### print

The `print-data` command will print a data frame in a nicely formatted
way, respecting the pretty printing row/column length variables:

```lisp
(print-data mtcars)
;; MODEL                MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;; Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;; Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;; Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
...
;; Output elided for brevity
```

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
;DUP

;;    A  B  C
;; 0 A1 A1 A1
;; 1 A1 A1 A1
;; 2 A3 B3 C3

```

Now remove duplicate rows 0 and 1:

```lisp
(df-remove-duplicates dup)
;; A  B  C
;; A1 A1 A1
;; A3 B3 C3
```

### remove data-frame

If you are working with large data sets, you may wish to remove a data
frame from your environment to save memory.  The `undef` command does
this:

```
LS-USER> (undef 'tooth-growth)
(TOOTH-GROWTH)
```

You can check that it was removed with the `show-data-frames`
function, or by viewing the list `df::*data-frames*`.

### list data-frames

To list the data frames in your environment, use the
`show-data-frames` function. Here is an example of what is currently
loaded into the authors environment. The data frames listed may be
different for you, depending on what you have loaded.

To see this output, you'll have to change to the standard
`print-object` method, using this code:

```lisp
(defmethod print-object ((df data-frame) stream)
  "Print DATA-FRAME dimensions and type
After defining this method it is permanently associated with data-frame objects"
  (print-unreadable-object (df stream :type t)
    (let ((description (and (slot-boundp df 'name)
			    (documentation (find-symbol (name df)) 'variable))))
    (format stream
	    "(~d observations of ~d variables)"
	    (aops:nrow df)
	    (aops:ncol df))
    (when description
      (format stream "~&~A" (short-string description))))))
```

Now, to see all the data frames in your environment:

```lisp
LS-USER> (show-data-frames)
#<DATA-FRAME AQ (153 observations of 7 variables)>

#<DATA-FRAME MTCARS (32 observations of 12 variables)
Motor Trend Car Road Tests>

#<DATA-FRAME USARRESTS (50 observations of 5 variables)
Violent Crime Rates by US State>

#<DATA-FRAME PLANTGROWTH (30 observations of 3 variables)
Results from an Experiment on Plant Growth>

#<DATA-FRAME TOOTHGROWTH (60 observations of 4 variables)
The Effect of Vitamin C on Tooth Growth in Guinea Pigs>
```

with the `:head t` option, `show-data-frames` will print the first
five rows of the data frame, similar to the `head` command:

```lisp
LS-USER> (show-data-frames :head t)
AQ
;;  X5             OZONE SOLAR-R WIND TEMP MONTH DAY
;;   1           41.0000     190  7.4   67     5   1
;;   2           36.0000     118  8.0   72     5   2
;;   3           12.0000     149 12.6   74     5   3
;;   4           18.0000     313 11.5   62     5   4
;;   5           42.1293      NA 14.3   56     5   5
;;   6           28.0000      NA 14.9   66     5   6 ..

MTCARS
;; MODEL                MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;; Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;; Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;; Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
;; Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
;; Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 ..

;; Output elided for brevity
```

You, of course, may see different output depending on what data frames
you currently have loaded.

Let's change the `print-object` back to our convenience method.

```lisp
(defmethod print-object ((df data-frame) stream)
  "Print the first six rows of DATA-FRAME"
  (let ((*print-lines* 6))
    (df:print-data df stream nil)))
```


## Column operations

You have seen some of these functions before, and for completeness we
repeat them here.

To obtain a variable (column) from a data frame, use the `column`
function.  Using the `mtcars-small` data frame, defined in [export data
frames](/docs/manuals/data-frame/#export-data-frames) above:

```lisp
(column mtcars-small 'mpg)
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
(columns mtcars-small '(mpg wt))
;; #(#(21 21 22.8d0 21.4d0 18.7d0) #(2.62d0 2.875d0 2.32d0 3.215d0 3.44d0))
```

### add columns

There are two 'flavors' of add functions, destructive and
non-destructive.  The latter return a **new** data frame as the
result, and the destructive versions modify the data frame passed as a
parameter.  The destructive versions are denoted with a '!' at the end
of the function name.

The columns to be added can be in several formats:

-  plist
-  alist
-  (plist)
-  (alist)
-  (data-frame)

To add a single column to a data frame, use the `add-column!`
function.  We'll use a data frame similar to the one used in our
reading data-frames from a string example to illustrate column
operations.

Create the data frame:

```lisp
(defparameter *d* (read-csv
		   (format nil "Gender,Age,Height
                      \"Male\",30,180
                      \"Male\",31,182
                      \"Female\",32,165
	                  \"Male\",22,167
	                  \"Female\",45,170")))
```
and print it:
```lisp
(head *d*)
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

;; GENDER AGE HEIGHT WEIGHT       BMI A    B
;; Male    30    180   75.2   23.2099 1  FOO
;; Male    31    182   88.5   26.7178 2  BAR
;; Female  32    165   49.4   18.1451 3  BAZ
;; Male    22    167   78.1   28.0039 4  QUX
;; Female  45    170   79.4   27.4740 5 QUUX
```


### remove columns

Let's remove the columns `a` and `b` that we just added above with
the `remove-columns` function.  Since it returns a new data frame,
we'll need to assign the return value to `*d*`:

```lisp
(setf *d* (remove-columns *d* '(a b bmi)))

;; GENDER AGE HEIGHT WEIGHT       BMI
;; Male    30    180   75.2   23.2099
;; Male    31    182   88.5   26.7178
;; Female  32    165   49.4   18.1451
;; Male    22    167   78.1   28.0039
;; Female  45    170   79.4   27.4740
```

To remove columns destructively, meaning modifying the original data,
use the `remove-column!` or `remove-columns!` functions.


### rename columns

Sometimes data sources can have variable names that we want to change.
To do this, use the `rename-column!` function.  This example will
rename the 'gender' variable to 'sex':

```lisp
(rename-column! *d* 'sex 'gender)

;;   SEX    AGE HEIGHT WEIGHT
;; 0 Male    30    180   75.2
;; 1 Male    31    182   88.5
;; 2 Female  32    165   49.4
;; 3 Male    22    167   78.1
;; 4 Female  45    170   79.4
```

If you used `defdf` to create your data frame, and this is the
recommended way to define data frames, the variable references within
the data package will have been updated.  This is true for all
destructive data frame operations.  Let's use this now to rename the
`mtcars` `X1` variable to `model`.  First a quick look at the first 2
rows as they are now:

```lisp
(head mtcars 2)
;;   X1                 MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
```

Replace `X1` with `model`:

```lisp
(rename-column! mtcars 'model 'x1)
```

Note: check to see what value your version of `mtcars` has. In this
case, with a fresh start of Lisp-Stat, it has `X1`. It could have
`X2`, `X3`, etc.

Now check that it worked:

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

### replace columns

Columns are "setf-able" places and the simplest way to replace a
column is set the field to a new value.  We'll complement the `sex`
field of `*d*`:

```lisp
(df::setf (df:column *d* 'sex) #("Female" "Female" "Male" "Female" "Male"))
;#("Female" "Female" "Male" "Female" "Male")
```

Note that `df::setf` is not exported. Use this with caution.

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
LS-USER> (print-data *d*)
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

### transform columns

There are two functions for column transformations, `replace-column`
and `map-columns`.

#### replace-column
`replace-column` can be used to transform a column by applying a
function to each value.  This example will add 20 to each row of the
`age` column:

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
on all the values of each variable/column as a vector, rather than the
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
above, so `*d*` has the original values.  Also note the use of `select`
to get the numeric variables from the data frame; `e+` can't add
categorical values like gender/sex.

## Row operations

As the name suggests, row operations operate on each row, or
observation, of a data set.

### add rows

Adding rows is done with the [array-operations stacking functions](/docs/manuals/array-operations/#stacking).  Since these functions operate on both arrays and data frames, we can use them to stack data frames, arrays, or a mixture of both, providing they have a rank of 2.  Here's an example of adding a row to the `mtcars` data frame:

```lisp
(defparameter boss-mustang
  #("Boss Mustang" 12.7d0 8 302 405 4.11d0 2.77d0 12.5d0 0 1 4 4))
```
and now stack it onto the `mtcars` data set (load it with `(data :mtcars)` if you haven't already done so):
```lisp
(matrix-df
 (keys mtcars)
 (stack-rows mtcars boss-mustang))
```
This is the functional equivalent of R's `rbind` function.  You can also add columns with the `stack-cols` function.

An often asked question is: why don't you have a dedicated `stack-rows` function?  Well, if you want one it might look like this:
```lisp
(defun stack-rows (df &rest objects)
  "Stack rows that works on matrices and/or data frames."
  (matrix-df
   (keys df)
   (apply #'aops:stack-rows (cons df objects))))
```
But now the data frame must be the first parameter passed to the function.  Or perhaps you want to rename the columns?  Or you have matrices as your starting point?  For all those reasons, it makes more sense to pass in the column keys than a data frame:
```lisp
(defun stack-rows (col-names &rest objects)
  "Stack rows that works on matrices and/or data frames."
  (matrix-df
   (keys col-names)
   (stack-rows objects)))
```
However this means we have two `stack-rows` functions, and you don't really gain anything except an extra function call.  So use the above definition if you like; we use the first example and call `matrix-df` and `stack-rows` to stack data frames.


### count-rows

This function is used to determine how many rows meet a certain
condition.  For example if you want to know how many cars have a MPG
(miles-per-galleon) rating greater than 20, you could use:

```lisp
(count-rows mtcars 'mpg #'(lambda (x) (< 20 x)))
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

### mask-rows

`mask-rows` is similar to `count-rows`, except it returns a bit-vector
for rows matching the predicate.  This is useful when you want to pass
the bit vector to another function, like `select` to retrieve only the
rows matching the predicate.

```lisp
(mask-rows mtcars 'mpg #'(lambda (x) (< 20 x)))
; => #*11110001100000000111100001110001
```

### filter-rows

The `filter-rows` function will return a `data-frame` whose rows match
the predicate.  The function signature is:

```lisp
(defun filter-rows (data body) ...
```

As an example, let's filter `mtcars` to find all the cars whose fuel
consumption is greater than 20 mpg:

```lisp
(filter-rows mtcars '(< 20 mpg))
;=> #<DATA-FRAME (14 observations of 12 variables)>
```

To view them we'll need to call the `print-data` function directly instead
of using the `print-object` function we installed earlier.  Otherwise,
we'll only see the first 6.

```lisp
(print-data *)
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

Filter predicates can be more complex than this, here's an example
filtering the Vega `movies` data set (which we call `imdb`):

```lisp
(filter-rows imdb
     '(and (not (eql imdb-rating :na))
	       (local-time:timestamp< release-date
	                              (local-time:parse-timestring "2019-01-01"))))
```

You can refer to any of the column/variable names in the `data-frame`
directly when constructing the filter predicate.  The predicate is
turned into a lambda function, so `let`, etc is also possible.

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
functions, see the [data-frame api reference](/docs/reference/data-frame/).

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

tail *data-frame* &optional *n*
: displays the last *n* rows of data-frame. *n* defaults to 6.


### describe

describe *data-frame*
: returns the meta-data for the variables in *data-frame*

`describe` is a common lisp function that describes an object.  In
Lisp-Stat `describe` prints a description of the data frame and the
three 'standard' properties of the variables: type, unit and
description.  It is similar to the `str` command in R. To see an
example use the augmented `mtcars` data set included in Lisp-Stat.  In
this data set, we have added properties describing the variables.
This is a good illustration of why you should always save data frames
in lisp format; properties such as these are lost in CSV format.

```lisp
(data :mtcars)
```

```lisp
LS-USER> (describe mtcars)
MTCARS
  Motor Trend Car Road Tests
  A data-frame with 32 observations of 12 variables

Variable | Type         | Unit | Label
-------- | ----         | ---- | -----------
MODEL    | STRING       | NIL  | NIL
MPG      | DOUBLE-FLOAT | M/G  | Miles/(US) gallon
CYL      | INTEGER      | NA   | Number of cylinders
DISP     | DOUBLE-FLOAT | IN3  | Displacement (cu.in.)
HP       | INTEGER      | HP   | Gross horsepower
DRAT     | DOUBLE-FLOAT | NA   | Rear axle ratio
WT       | DOUBLE-FLOAT | LB   | Weight (1000 lbs)
QSEC     | DOUBLE-FLOAT | S    | 1/4 mile time
VS       | BIT          | NA   | Engine (0=v-shaped, 1=straight)
AM       | BIT          | NA   | Transmission (0=automatic, 1=manual)
GEAR     | INTEGER      | NA   | Number of forward gears
CARB     | INTEGER      | NA   | Number of carburetors
```

### summary

summary *data-frame*
: returns a summary of the variables in *data-frame*

Summary functions are one of those things that tend to be use-case or application specific. Witness the number of R summary packages; there are at least half a dozen, including [hmisc](https://www.rdocumentation.org/packages/Hmisc/versions/4.1-0/topics/describe), [stat.desc](https://www.rdocumentation.org/packages/pastecs/versions/1.3.21/topics/stat.desc), [psych describe](https://www.rdocumentation.org/packages/pastecs/versions/1.3-18/topics/stat.desc), [skim](https://www.rdocumentation.org/packages/skimr/versions/1.0/topics/skim) and [summary tools](https://github.com/dcomtois/summarytools). In short, there is no one-size-fits-all way to provide summaries, so Lisp-Stat provides the data structures upon which users can customise the summary output. The output you see below is a simple `:print-function` for each of the summary structure types (numeric, factor, bit and generic).

```lisp
LS-USER> (summary mtcars)
(

MPG (Miles/(US) gallon)
 n: 32
 missing: 0
 min=10.40
 q25=15.40
 q50=19.20
 mean=20.09
 q75=22.80
 max=33.90

CYL (Number of cylinders)
14 (44%) x 8, 11 (34%) x 4, 7 (22%) x 6,

DISP (Displacement (cu.in.))
 n: 32
 missing: 0
 min=71.10
 q25=120.65
 q50=205.87
 mean=230.72
 q75=334.00
 max=472.00

HP (Gross horsepower)
 n: 32
 missing: 0
 min=52
 q25=96.00
 q50=123
 mean=146.69
 q75=186.25
 max=335

DRAT (Rear axle ratio)
 n: 32
 missing: 0
 min=2.76
 q25=3.08
 q50=3.70
 mean=3.60
 q75=3.95
 max=4.93

WT (Weight (1000 lbs))
 n: 32
 missing: 0
 min=1.51
 q25=2.54
 q50=3.33
 mean=3.22
 q75=3.68
 max=5.42

QSEC (1/4 mile time)
 n: 32
 missing: 0
 min=14.50
 q25=16.88
 q50=17.71
 mean=17.85
 q75=18.90
 max=22.90

VS (Engine (0=v-shaped, 1=straight))
ones: 14 (44%)

AM (Transmission (0=automatic, 1=manual))
ones: 13 (41%)

GEAR (Number of forward gears)
15 (47%) x 3, 12 (38%) x 4, 5 (16%) x 5,

CARB (Number of carburetors)
10 (31%) x 4, 10 (31%) x 2, 7 (22%) x 1, 3 (9%) x 3, 1 (3%) x 6, 1 (3%) x 8, )

```

Note that the `model` column, essentially `row-name` was deleted from
the output. The `summary` function, designed for human readable
output, removes variables with all unique values, and those with
monotonically increasing numbers (usually row numbers).

To build your own summary function, use the `get-summaries` function
to get a list of summary structures for the variables in the data
frame, and then print them as you wish.

### columns

You can also describe or summarize individual columns:

```lisp
LS-USER> (describe 'mtcars:mpg)
MTCARS:MPG
  [symbol]

MPG names a symbol macro:
  Expansion: (AREF (COLUMNS MTCARS) 1)

Symbol-plist:
  :TYPE -> DOUBLE-FLOAT
  :UNIT -> M/G
  :LABEL -> "Miles/(US) gallon"
```

```lisp
LS-USER> (summarize-column 'mtcars:mpg)

MPG (Miles/(US) gallon)
 n: 32
 missing: 0
 min=10.40
 q25=15.40
 q50=19.20
 mean=20.09
 q75=22.80
 max=33.90
```

## Missing values

Data sets often contain missing values and we need to both understand
where and how many are missing, and how to transform or remove them
for downstream operations.  In Lisp-Stat, missing values are
represented by the keyword symbol `:na`.  You can control this
encoding during delimited text import by passing an `a-list`
containing the mapping.  By default this is a keyword parameter
`map-alist`:

```lisp
(map-alist '((""   . :na)
             ("NA" . :na)))
```

The default maps blank cells ("") and ones containing "NA" (not
available) to the keyword `:na`, which stands for missing.
Some systems encode missing values as numeric, e.g. `99`; in this case
you can pass in a `map-alist` that includes this mapping:

```lisp
(map-alist '((""   . :na)
             ("NA" . :na)
			 (99   . :na)))
```

We will use the R air-quality dataset to illustrate working with
missing values.  Let's load it now:

```lisp
(defdf aq
  (read-csv rdata:airquality))
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
 (print-data (missingp aq))

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
LS-USER> (summary (missingp aq))
X4: 153 (100%) x NIL,
OZONE: 116 (76%) x NIL, 37 (24%) x T,
SOLAR-R: 146 (95%) x NIL, 7 (5%) x T,
WIND: 153 (100%) x NIL,
TEMP: 153 (100%) x NIL,
MONTH: 153 (100%) x NIL,
DAY: 153 (100%) x NIL,
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

Normally we'd round `mean` to be consistent from a type perspective,
but did not here so you can see the values that were replaced.

## Sampling

You can take a random sample of the rows of a data-frame with the `select:sample` function:

```lisp
LS-USER> mtcars
#<DATA-FRAME (32 observations of 12 variables)
Motor Trend Car Road Tests>
LS-USER> (sample mtcars 3 :skip-unselected t)
#<DATA-FRAME (3 observations of 12 variables)>
LS-USER> (print-data *)

;;   MODEL              MPG CYL  DISP  HP DRAT   WT  QSEC VS AM GEAR CARB
;; 0 Hornet Sportabout 18.7   8 360.0 175 3.15 3.44 17.02  0  0    3    2
;; 1 Duster 360        14.3   8 360.0 245 3.21 3.57 15.84  0  0    3    4
;; 2 Merc 230          22.8   4 140.8  95 3.92 3.15 22.90  1  0    4    2
```

You can also take random samples from CL sequences and arrays, with or without replacement and in various proportions.  For further information see [sampling](/docs/manuals/select/#sampling) in the [select system manual](/docs/manuals/select/).

Uses [Vitter's Algorithm
D](http://www.ittc.ku.edu/~jsv/Papers/Vit87.RandomSampling.pdf) to
efficiently select the rows.  Sometimes you may want to use the
algorithm at a lower level. If you don’t want the sample itself, say you
only want the indices, you can directly use `map-random-below`, which
simply calls a provided function on each index.

This is an enhancement and port to standard common lisp of
ruricolist's
[random-sample](https://github.com/ruricolist/random-sample/tree/master).
It also removes the dependency on Trivia, which has a restrictive
license (LLGPL).

## Dates & Times

Lisp-Stat uses [localtime](https://github.com/dlowe-net/local-time) to
represent dates.  This works well, but the system is a bit strict on
input formats, and real-world data can be quite messy at times.  For
these cases [chronicity](https://github.com/chaitanyagupta/chronicity)
and
[cl-date-time-parser](https://github.com/tkych/cl-date-time-parser)
can be helpful.  Chronicity returns `local-time` `timestamp` objects,
and is particularly easy to work with.

For example, if you have a variable with dates encoded like: 'Jan 7
1995', you can recode the column like we did for the vega `movies`
data set:

```lisp
(replace-column! imdb 'release-date #'(lambda (x)
				       (local-time:universal-to-timestamp
					(date-time-parser:parse-date-time x))))
```
