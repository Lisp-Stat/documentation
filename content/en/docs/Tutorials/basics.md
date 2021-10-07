---
title: "Basics"
date: 2021-02-20
weight: 4
description: >
  An introduction to the basics of LISP-STAT
---

## Preface {#preface .unnumbered}

This document is intended to be a tutorial introduction to the basics
of LISP-STAT and is based on the original tutorial for XLISP-STAT
written by Luke Tierney, updated for Common Lisp and the 2021
implementation of LISP-STAT.

LISP-STAT is a statistical environment built on top of the Common Lisp
general purpose programming language.  The first three sections
contain the information you will need to do elementary statistical
calculations and plotting.  The fourth section introduces some
additional methods for generating and modifying data.  The fifth
section describes some features of the user interface that may be
helpful.  The remaining sections deal with more advanced topics, such
as interactive plots, regression models, and writing your own
functions.  All sections are organized around examples, and most
contain some suggested exercises for the reader.

This document is not intended to be a complete manual.  However,
documentation for many of the commands that are available is given in
the appendix.  Brief help messages for these and other commands are also
available through the interactive help facility described in
[Section 5.1](#Getting-Help) below.

Common Lisp (CL) is a dialect of the Lisp programming language,
published in ANSI standard document ANSI INCITS 226-1994 (S20018)
(formerly X3.226-1994 (R1999)).  The Common Lisp language was
developed as a standardized and improved successor of Maclisp. By the
early 1980s several groups were already at work on diverse successors
to MacLisp: Lisp Machine Lisp (aka ZetaLisp), Spice Lisp, NIL and S-1
Lisp. Common Lisp sought to unify, standardize, and extend the
features of these MacLisp dialects.  Common Lisp is not an
implementation, but rather a language specification.  Several
implementations of the Common Lisp standard are available, including
free and open-source software and proprietary products.  Common Lisp
is a general-purpose, multi-paradigm programming language.  It
supports a combination of procedural, functional, and object-oriented
programming paradigms.  As a dynamic programming language, it
facilitates evolutionary and incremental software development, with
iterative compilation into efficient run-time programs.  This
incremental development is often done interactively without
interrupting the running application.


### Using this Tutorial {#using-this-tutorial .unnumbered}

The best way to learn about a new computer programming language is
usually to use it.  You will get most out of this tutorial if you read
it at your computer and work through the examples yourself. To make
this tutorial easier the named data sets used in this tutorial have
been stored in the file `basic.lisp` in the `LS:DATASETS;TUTORIALS`
folder of the system.  To load this file, execute:

```lisp
(load #P"LS:DATASETS;TUTORIALS;basic")
```

at the command prompt (REPL). The file will be loaded and some
variables will be defined for you.

### Why LISP-STAT Exists {#why-lisp-stat-exists .unnumbered}

There are three primary reasons behind the decision to produce the
LISP-STAT environment. The first is speed. The other major languages
used for statistics and numerical analysis, R, Python and Julia are
all fine languages, but with the rise of 'big data' and large data
sets, require workarounds for processing large data sets. Furthermore,
as interpreted languages, they are relatively slow when compared to
Common Lisp, that has a compiler that produces native machine code.

Not only does Common Lisp provide a compiler that produces machine
code, it has native threading, a rich ecosystem of code libraries, and
a history of industrial deployments, including:

- Credit card authorization at AMEX (Authorizers Assistant)
- US DoD logistics (and more, that we don't know of)
- CIA and NSA are big users based on Lisp sales
- DWave and Rigetti use lisp for programming their quantum computers
- Apple's Siri was originally written in Lisp
- Amazon got started with Lisp & C; so did Y-combinator
- Google's flight search engine is written in Common Lisp
- AT&T used a stripped down version of Symbolics Lisp to process CDRs in the first IP switches

Python and R are never (to my knowledge) deployed as front-line
systems, but used in the back office to produce models that are
executed by other applications in enterprise environments. Common Lisp
eliminates that friction.

### Availability {#availability .unnumbered}

Source code for LISP-STAT is available in the [Lisp-Stat github
repository](https://github.com/Lisp-Stat).  The [Getting
Started](/docs/getting-started/) section of the
documentation contains instructions for downloading and installing the
system.


### Disclaimer {#disclaimer .unnumbered}

LISP-STAT is an experimental program.  Although it is in daily use on
several projects, the corporate sponsor, Symbolics Pte Ltd, takes no
responsibility for losses or damages resulting directly or indirectly
from the use of this program.

LISP-STAT is an evolving system.  Over time new features will be
introduced, and existing features that do not work may be changed.
Every effort will be made to keep LISP-STAT consistent with the
information in this tutorial, but if this is not possible the
[reference documentation](/docs/reference) should give accurate
information about the current use of a command.

## Starting and Finishing

Once you have obtained the source code or pre-built image, you can
load Lisp-Stat using [QuickLisp](https://www.quicklisp.org/beta/).  If
you do not have quicklisp, stop here and get it.  It is the de-facto
package manager for Common Lisp and you will need it.  This is what
you will see if loading using the
[Slime](https://common-lisp.net/project/slime/) IDE:

```lisp
CL-USER> (asdf:load-system :lisp-stat)
To load "lisp-stat":
  Load 1 ASDF system:
    lisp-stat
; Loading "lisp-stat"
..................................................
..................................................
[package num-utils]...............................
[package num-utils]...............................
[package dfio.decimal]............................
[package dfio.string-table].......................
.....
(:LISP-STAT)
CL-USER>
```

You may see more or less output, depending on whether dependent
packages have been compiled before.  If this is your first time
running anything in this implementation of Common Lisp, you will
probably see output related to the compilation of every module in the
system.  This could take a while, but only has to be done once.

Once completed, to use the functions provided, you need to make the
LISP-STAT package the current package, like this:

```lisp
(in-package :ls-user)
#<PACKAGE "LS-USER">
LS-USER>
```

The final `LS-USER>` in the window is the Slime prompt. Notice how it
changes when you executed `(in-package)`. In Slime, the prompt always
indicates the current package, `*package*`.  Any characters you type
while the prompt is active will be added to the line after the final
prompt.  When you press *return*, LISP-STAT will try to interpret what
you have typed and will print a response.  For example, if you type a
1 and press *return* then LISP-STAT will respond by simply printing a
1 on the following line and then give you a new prompt:

```lisp
    LS-USER> 1
    1
    LS-USER>
```

If you type an *expression* like `(+ 1 2)`, then LISP-STAT will
print the result of evaluating the expression and give you a new prompt:

```lisp
    LS-USER> (+ 1 2)
    3
    LS-USER>
```

As you have probably guessed, this expression means that the numbers 1
and 2 are to be added together.  The next section will give more
details on how LISP-STAT expressions work.  In this tutorial I will
sometimes show interactions with the program as I have done here: The
`LS-USER>` prompt will appear before lines you should type.
LISP-STAT will supply this prompt when it is ready; you should not
type it yourself.  In later sections I will omit the new prompt
following the result in order to save space.

Now that you have seen how to start up LISP-STAT it is a good idea to
make sure you know how to get out.  The exact command to exit depends
on the Common Lisp implementation you use. For SBCL, you can type the
expression

```lisp
    LS-USER> (exit)
```

In other implementations, the command is `quit`.  One of these methods
should cause the program to exit and return you to the IDE.  In Slime,
you can use the `,` short-cut and then type `sayoonara`.

## The Basics

Before we can start to use LISP-STAT for statistical work we need to
learn a little about the kind of data LISP-STAT uses and about how the
LISP-STAT *listener* and *evaluator* work.

### Data

LISP-STAT works with two kinds of data: *simple data* and *compound
data*. Simple data are numbers

    1                   ; an integer
    -3.14               ; a floating point number
    #C(0 1)             ; a complex number (the imaginary unit)

logical values

    T                   ; true
    nil                 ; false

strings (always enclosed in double quotes)

    "This is a string 1 2 3 4"

and symbols (used for naming things; see the following section)

    x
    x12
    12x
    this-is-a-symbol

Compound data are lists

    (this is a list with 7 elements)
    (+ 1 2 3)
    (sqrt 2)

or vectors

    #(this is a vector with 7 elements)
    #(1 2 3)

Higher dimensional arrays are another form of compound data; they will
be discussed below in [Section 9](#Arrays), "Arrays".

All the examples given above can be typed directly into the command
window as they are shown here. The next subsection describes what
LISP-STAT will do with these expressions.

<!--
Move this elsewhere. Not strictly needed in the basic tutorial
### Data Frame

A data frame is a collection of name/data pairs.  If you have used R,
then you'll already be familiar with this concept.  To create a data
frame from a name and a value (called a `plist`, or *property-list*):

```lisp
(plist-df '(name #(1 2 3)))

#<DATA-FRAME (3 observations of 1 variables)>
```
-->

### The Listener and the Evaluator

A session with LISP-STAT basically consists of a conversation between
you and the *listener*.  The listener is the window into which you
type your commands.  When it is ready to receive a command it gives
you a prompt.  At the prompt you can type in an expression.  You can
use the mouse or the *backspace* key to correct any mistakes you make
while typing in your expression.  When the expression is complete and
you type a *return* the listener passes the expression on to the
*evaluator*.  The evaluator evaluates the expression and returns the
result to the listener for printing.[^1] The evaluator is the heart of
the system.

The basic rule to remember in trying to understand how the evaluator
works is that everything is evaluated. Numbers and strings evaluate to
themselves:

    LS-USER> 1
    1
    LS-USER> "Hello"
    "Hello"
    LS-USER>

Lists are more complicated. Suppose you type the list `(+ 1 2 3)`
at the listener. This list has four elements: the symbol `+`
followed by the numbers 1, 2 and 3. Here is what happens:

    > (+ 1 2 3)
    6
    >

This list is evaluated as a function application.  The first element
is a symbol representing a function, in this case the symbol `+`
representing the addition function.  The remaining elements are the
arguments.  Thus the list in the example above is interpreted to mean
"Apply the function `+` to the numbers 1, 2 and 3".

Actually, the arguments to a function are always evaluated before the
function is applied. In the previous example the arguments are all
numbers and thus evaluate to themselves. On the other hand, consider

    LS-USER> (+ (* 2 3) 4)
    10
    LS-USER>

The evaluator has to evaluate the first argument to the function
`+` before it can apply the function.

Occasionally you may want to tell the evaluator *not* to evaluate
something.  For example, suppose we wanted to get the evaluator to simply
return the list `(+ 1 2)` back to us, instead of evaluating it. To
do this we need to *quote* our list:

    LS-USER> (quote (+ 1 2))
    (+ 1 2)
    LS-USER>

`quote` is not a function. It does not obey the rules of function
evaluation described above: Its argument is not evaluated. `quote` is
called a *special form* -- special because it has special rules for
the treatment of its arguments.  There are a few other special forms
that we will need; I will introduce them as they are needed.  Together
with the basic evaluation rules described here these special forms
make up the basics of the Lisp language.  The special form `quote` is
used so often that a shorthand notation has been developed, a single
quote before the expression you want to quote:

    LS-USER> '(+ 1 2)      ; single quote shorthand

This is equivalent to `(quote (+ 1 2))`.  Note that there is no
matching quote following the expression.

By the way, the semicolon `;` is the Lisp comment character.
Anything you type after a semicolon up to the next time you press
*return* is ignored by the evaluator.


### Exercises {#exercises .unnumbered}

For each of the following expressions try to predict what the evaluator
will return. Then type them in, see what happens and try to explain any
differences.

1.  `(+ 3 5 6)`

2.  `(+ (- 1 2) 3)`

3.  `’(+ 3 5 6)`

4.  `’( + (- 1 2) 3)`

5.  `(+ (- (* 2 3) (/ 6 2)) 7)`

6.  `’x`

Remember, to quit from LISP-STAT type `(exit)`, `quit` or use the
IDE's exit mechanism.

## Elementary Statistical Operations {#Elementary}

This section introduces some of the basic graphical and numerical
statistical operations that are available in LISP-STAT.

### First Steps {#Elementary.First}

Statistical data usually consists of groups of numbers. Devore and Peck
[@DevorePeck Exercise 2.11] describe an experiment in which 22 consumers
reported the number of times they had purchased a product during the
previous 48 week period. The results are given as a table:

  --- --- --- --- --- --- --- --- --- --- ---
    0   2   5   0   3   1   8   0   3   1   1
    9   2   4   0   2   9   3   0   1   9   8
  --- --- --- --- --- --- --- --- --- --- ---

To examine this data in LISP-STAT we represent it as a list of numbers
using the `list` function:

```lisp
(list 0 2 5 0 3 1 8 0 3 1 1 9 2 4 0 2 9 3 0 1 9 8)
```

{{< alert title="Note" >}}The text boxes above have a 'copy' button if
you hover on them.  For some examples, I will give the commands alone in
the text box so that you can copy & paste the code into the REPL{{<
/alert >}}

Note that the numbers are separated by white space (spaces, tabs or even
returns), not commas.

The `mean` function can be used to compute the average of a list of
numbers. We can combine it with the `list` function to find the
average number of purchases for our sample:

```lisp
(mean '(0 2 5 0 3 1 8 0 3 1 1 9 2 4 0 2 9 3 0 1 9 8)) ; => 3.227273
```

The median of these numbers can be computed as

```lisp
(median '(0 2 5 0 3 1 8 0 3 1 1 9 2 4 0 2 9 3 0 1 9 8)) ; => 2
```

It is of course a nuisance to have to type in the list of 22 numbers
every time we want to compute a statistic for the sample. To avoid
having to do this I will give this list a name using the `def`
special form [^2]:

```lisp
(def purchases (list 0 2 5 0 3 1 8 0 3 1 1 9 2 4 0 2 9 3 0 1 9 8))
; PURCHASES
```

Now the symbol `purchases` has a value associated with it:  Its
value is our list of 22 numbers. If you give the symbol `purchases`
to the evaluator then it will find the value of this symbol and return
that value:

    LS-USER> purchases
    (0 2 5 0 3 1 8 0 3 1 1 9 2 4 0 2 9 3 0 1 9 8)

{{< alert title="Note" >}}Common Lisp provides two functions to define
variables `defparameter` and `defvar`.  Variables defined with
`defparameter` can be modified without a warning. If you attempt to
modify a variable defined with `defvar` a warning will be issued and
you will have to confirm the change. {{< /alert >}}

We can now easily compute various numerical descriptive statistics for
this data set:

    LS-USER> (mean purchases)
    3.227273
    LS-USER> (median purchases)
    2
    LS-USER> (standard-deviation purchases)
    3.2041426
    LS-USER> (interquartile-range purchases)
    4

LISP-STAT also supports elementwise arithmetic operations on vectors
of numbers.  Technically, overriding, or 'shadowing' any of the Common
Lisp functions is *undefined*.  This is usually an euphuism for
'something really bad will happen', so the vector functions are
located in the package `elmt` and prefixed by `e` to distinguish them
from the Common Lisp variants, e.g. `e+` for addition, `e*` for
multiplication, etc.  Presently these functions work only on vectors,
so we'll define a new purchases variable as a vector type:

```lisp
(def purchases-2 #(0 2 5 0 3 1 8 0 3 1 1 9 2 4 0 2 9 3 0 1 9 8))
```

The `#` symbol tells the *listener* to interpret the list as a vector,
much like the `'` signals a list.

{{< alert color="warning" title="Warning" >}}Lists are fine for small
data-sets, but can rapidly cause memory exhaustion when they are
large. Get into the habit of using vectors with Lisp-Stat {{< /alert >}}

Now we can add 1 to each of the purchases:

```
LS-USER> (e+ 1 purchases-2)
    (1 3 6 1 4 2 9 1 4 2 2 10 3 5 1 3 10 4 1 2 10 9)
```

and after adding 1 we can compute the natural logarithms of the results:

    LS-USER> (elog (e+ 1 purchases-2))
    (0 1.098612 1.791759 0 1.386294 0.6931472 2.197225 0 1.386294 0.6931472
    0.6931472 2.302585 1.098612 1.609438 0 1.098612 2.302585 1.386294 0
    0.6931472 2.302585 2.197225)

{{< alert title="Note" >}}Using the `e` prefix for mathematical
operators is a temporary situation. We know how to merge vectorized
mathematics into the base Common Lisp, but since we have a functioning
system, this work is lower priority. Volunteers to take this on are
welcome.{{< /alert >}}




### Exercises {#exercises-1 .unnumbered}

For each of the following expressions try to predict what the evaluator
will return. Then type them in, see what happens and try to explain any
differences.

1.  `(mean (list 1 2 3))`

2.  `(e+ #(1 2 3) 4)`

3.  `(e* #(1 2 3) #(4 5 6))`

4.  `(e+ #(1 2 3) #(4 5 7))`


### Summary Statistics {#Summary}

Devore and Peck [@DevorePeck page 54, Table 10] give precipitation
levels recorded during the month of March in the Minneapolis - St. Paul
area over a 30 year period. Let's enter these data into LISP-STAT with
the name `precipitation`:

```lisp
(def precipitation
    #(.77 1.74 .81 1.20 1.95 1.20 .47 1.43 3.37 2.20 3.30
     3.09 1.51 2.10 .52 1.62 1.31 .32 .59 .81 2.81 1.87
     1.18 1.35 4.75 2.48 .96 1.89 .90 2.05))
```

In typing the expression above I have inserted *return* and *tab* a
few times in order to make the typed expression easier to read.  The
tab key indents the next line to a reasonable point to make the
expression more readable.

Here are some numerical summaries:

    LS-USER> (mean precipitation)
    1.685
    LS-USER> (median precipitation)
    1.47
    LS-USER> (standard-deviation precipitation)
    1.0157
    LS-USER> (interquartile-range precipitation)
    1.145


The distribution of this data set is somewhat skewed to the right.
Notice the separation between the mean and the median.  You might want
to try a few simple transformations to see if you can symmetrize the
data.  Square root and log transformations can be computed using the
expressions

```lisp
(esqrt precipitation)
```

and

```lisp
(elog precipitation)
```

You should look at plots of the data to see if these transformations do
indeed lead to a more symmetric shape.  The means and medians of the
transformed data are:

```lisp
    LS-USER> (mean (esqrt precipitation))
    1.243006
    LS-USER> (median (esqrt precipitation))
    1.212323
    LS-USER> (mean (elog precipitation))
    0.3405517
    LS-USER> (median (elog precipitation))
    0.384892
```

<!-- Remove this until version 2.0 of plotting is complete
### Plots

For this section we'll be using the Vega-Lite plotting back-end. Load
it like this:

```lisp
(asdf:load-system :plot/vglt)
```

The `histogram` and `box-plot` functions can be used to obtain
graphical representations of this data set:

```lisp
(vglt:plot
	(vglt:histogram
		(plist-df `(x ,precipitation)) "X" :title "Histogram of precipitation levels"))
```

{{< figure src="/docs/tutorials/figure-1-histogram.png" >}}

Note how we converted the precipitation data into a data-frame before
passing it to the `histogram` function.  This is because plotting
functions work on data frames. Also note the way the data frame was
constructed using the `plist-df` function.  When I first showed you an
example of constructing a data frame:

```lisp
(plist-df '(name #(1 2 3)))
```

the second value of the plist was a vector. In the histogram plot, the
second value is a *variable*:

```lisp
(plist-df `(x ,precipitation))
```

If you entered this into the evaluator (REPL) without the back-quote
and comma:

```lisp
(plist-df '(x precipitation))
```

you would get an error. This is because within a list, `precipitation`
is a *symbol*, and `plist-df` expects the vector that `precipitation`
stands for, in other words its *value*. To get the value, we use a
sort of template mechanism, that starts with the back-quote character.
Within a list that starts with this character, a comma signals to the
evaluator to put the *value* of the symbol there, not the symbol
itself.  The easiest way to see this is to type both into the
evaluator:

```
LS-USER> '(x precipitation)
(X PRECIPITATION)
LS-USER> `(x ,precipitation)
(X
 #(0.77 1.74 0.81 1.2 1.95 1.2 0.47 1.43 3.37 2.2 3.3 3.09 1.51 2.1 0.52 1.62
   1.31 0.32 0.59 0.81 2.81 1.87 1.18 1.35 4.75 2.48 0.96 1.89 0.9 2.05))
```

Note each graph is saved to an HTML file in your system cache
directory. This location will vary depending on your operating system.
On MS Windows, it will be in %APPDATALOCAL%/cache.  You can view or
edits the plots directly if you like.

Let's try a box plot:

```lisp
(vglt:plot
 (vglt::box-plot
  (plist-df `(x ,precipitation)) nil "X" :title "Boxplot of precipitation levels"))
```

{{< figure src="/docs/tutorials/figure-2-boxplot.png" >}}

The box-plot function can also be used to produce parallel box-plots of
two or more samples.

It will do so if it is given a list of lists as its
argument instead of a single list.

As an example, let's use this function to compare the fuel consumption
for various automobile types.  The data comes from the R `ggplot`
library and we load it like this:

```lisp
(defdf mpg (read-csv rdata:mpg)
  "Fuel economy data from 1999 to 2008 for 38 popular models of cars")
```

The parallel box-plot is obtained by:

```lisp
(vglt:plot
	  (vglt:box-plot mpg "CLASS" "HWY"
	                 :title "Boxplot of fuel consumption"))
```

{{< figure src="/docs/tutorials/figure-3-parallel-box-plot.png" >}}



### Exercises {#exercises-2 .unnumbered}

The following exercises involve examples and problems from Devore and
Peck. The data sets are in files in the folder **Datasets** in the
LISP-STAT distribution directory and can be read in using the `load`
command.  The short cut for the *Datasets* directory is `LS:DATASETS`,
so to load `car-prices`, type:

```lisp
(load #P"LS:DATASETS;car-prices")
```

at the REPL.  The file will be loaded and some variables will be
defined for you. Loading file `car-prices.lisp` will define the single
variable `car-prices`.  Loading file `heating.lisp` will define two
variables, `gas-heat` and `electric-heat`.[^3]

1.  Devore and Peck [@DevorePeck page 18, Example 2] give advertised
    prices for a sample of 50 used Japanese subcompact cars. [Create a
    data-frame](/docs/tasks/data-frame/#create-data-frames) and obtain
    some plots and summary statistics for this data.  Experiment with
    some transformations of the data as well.  The data set is called
    `car-prices` in the file `car-prices.lisp`.  The prices are given
    in units of \$1000; thus the price 2.39 represents \$2390.  The
    data have been sorted by their leading digit.

2.  In Exercise 2.40 Devore and Peck [@DevorePeck] give heating costs
    for a sample of apartments heated by gas and a sample of
    apartments heated by electricity.  Create a data-frame and obtain
    plots and summary statistics for these samples separately and look
    at a parallel box plot for the two samples.  These data sets are
    called `gas-heat` and `electric-heat` in the file `heating.lisp`.
-->
<!--
### Two Dimensional Plots {#Elementary.TwoDPlots}

Many single samples are actually collected over time.  The
precipitation data set used above is an example of this kind of data.
In some cases it is reasonable to assume that the observations are
independent of one another, but in other cases it is not.  One way to
check the data for some form of serial correlation or trend is to plot
the observations against time, or against the order in which they were
obtained.  I will use the `plot-points` function to produce a
scatter-plot of the precipitation data versus time.  The `plot-points`
function is called as

    (plot-points x-variable y-variable)

Our $y$-variable will be `precipitation`, the variable we defined
earlier.  As our $x$-variable we would like to use a sequence of
integers from 1 to 30.  We could type these in ourselves, but there is
an easier way.  The function `iseq`, short for *integer-sequence*,
generates a list of consecutive integers between two specified
values. The general form for a call to this function is

    (iseq start end).

To generate the sequence we need we use

    (iseq 1 30).

Thus to generate the scatter plot we type

    > (plot-points (iseq 1 30) precipitation)
    #<Object: 3423466, prototype = SCATTERPLOT-PROTO>
    >

and the result will look like Figure
[\[Scatterplot1\]](#Scatterplot1){reference-type="ref"
reference="Scatterplot1"}.

There does not appear to be much of a pattern to the data; an
independence assumption may be reasonable.

Sometimes it is easier to see temporal patterns in a plot if the points
are connected by lines.  Try the above command with `plot-points`
replaced by `plot-lines`.

The `plot-lines` function can also be used to construct graphs of
functions. Suppose you would like a plot of $\sin(x)$ from $-\pi$ to
$+\pi$. The constant $\pi$ is predefined as the variable `pi`. You
can construct a list of $n$ equally spaced real numbers between $a$ and
$b$ using the expression

    (rseq a b n).

Thus to draw the plot of $\sin(x)$ using 50 equally spaced points type

    > (plot-lines (rseq (- pi) pi 50) (sin (rseq (- pi) pi 50)))
    #<Object: 3423466, prototype = SCATTERPLOT-PROTO>
    >

The plot should look like Figure
[\[Lineplot1\]](#Lineplot1){reference-type="ref" reference="Lineplot1"}.

Scatter-plots are of course particularly useful for examining the
relationship between two numerical observations taken on the same
subject. Devore and Peck [@DevorePeck Exercise 2.33] give data for HC
and CO emission recorded for 46 automobiles. The results can be placed
in two variables, `hc` and `co`, and these variable can then
be plotted against one another with the `plot-points` function:

    > (def hc (list .5 .46 .41 .44 .72 .83 .38 .60 .83 .34 .37 .87
                    .65 .48 .51 .47 .56 .51 .57 .36 .52 .58 .47 .65
                    .41 .39 .55 .64 .38 .50 .73 .57 .41 1.02 1.10 .43
                    .41 .41 .52 .70 .52 .51 .49 .61 .46 .55))
    HC
    > (def co (list 5.01 8.60 4.95 7.51 14.59 11.53 5.21 9.62 15.13
                    3.95 4.12 19.00 11.20 3.45 4.10 4.74 5.36 5.69
                    6.02 2.03 6.78 6.02 5.22 14.67 4.42 7.24 12.30
                    7.98 4.10 12.10 14.97 5.04 3.38 23.53 22.92 3.81
                    1.85 2.26 4.29 14.93 6.35 5.79 4.62 8.43 3.99 7.47))
    CO
    > (plot-points hc co)
    #<Object: 3423466, prototype = SCATTERPLOT-PROTO>
    >

The resulting plot is shown in Figure
[\[Scatterplot2\]](#Scatterplot2){reference-type="ref"
reference="Scatterplot2"}.

### Exercises {#exercises-3 .unnumbered}

1.  Draw a graph of the function $f(x) = 2x + x^{2}$ between -2 and 3.

2.  Devore and Peck [@DevorePeck Exercise 4.2] give the age and CPK
    concentration, a measure of metabolic activity, recorded for 18
    cross country skiers during a relay race. These data are in the
    variables `age` and `cpk` in the file
    `metabolism.lsp`. Plot the data and describe any relationship
    you observe between age and CPK concentration.

### Plotting Functions

Plotting the sine function in the previous section was a bit cumbersome.
As an alternative we can use the function `plot-function` to plot a
function of one argument over a specified range. We can plot the sine
function using the expression

    (plot-function (function sin) (- pi) pi)

The expression `(function sin)` is needed to extract the function
associated with the symbol `sin`. Just using `sin` will not
work. The reason is that a symbol in Lisp can have both a *value*,
perhaps set using `def`, and a *function definition* at the same
time. [^4] This may seem a bit cumbersome at first, but it has one great
advantage: Typing an innocent expression like

    (def list '(2 3 4))

will not destroy the `list` function.

Extracting a function definition from a symbol is done almost as often
as quoting an expression, so again a simple shorthand notation is
available. The expression

    #'sin

is equivalent to the expression `(function sin)`. The short form
`#’` is usually pronounced *sharp-quote*. Using this abbreviation
the expression for producing the sine plot can be written as

    (plot-function #'sin (- pi) pi).
-->


## Generating and Modifying Data

This section briefly summarizes some techniques for generating random
and systematic data.


### Generating Random Data
<!--
LISP-STAT has several functions for generating pseudo-random numbers
in the [distributions](https://github.com/Lisp-Stat/distributions)
system, loaded as part of LISP-STAT.  For example, the expression

    (uniform-rand 50)

will generate a list of 50 independent uniform random variables. The
functions `normal-rand` and `cauchy-rand` work similarly.
Other generating functions require additional arguments to specify
distribution parameters. Here is a list of the functions available for
dealing with probability distributions:

| CDF          | Quantile       | Draw          | PDF          |
| ---          | ---            | ---           | ---          |
| normal-cdf   | normal-quant   | normal-rand   | normal-dens  |
| cauchy-cdf   | cauchy-quant   | cauchy-rand   | cauchy-dens  |
| beta-cdf     | beta-quant     | beta-rand     | beta-dens    |
| gamma-cdf    | gamma-quant    | gamma-rand    | gamma-dens   |
| chisq-cdf    | chisq-quant    | chisq-rand    | chisq-dens   |
| t-cdf        | t-quant        | t-rand        | t-dens       |
| f-cdf        | f-quant        | f-rand        | f-dens       |
| binomial-cdf | binomial-quant | binomial-rand | binomial-pmf |
| poisson-cdf  | poisson-quant  | poisson-rand  | poisson-pmf  |
| bivnorm-cdf  |                |               |              |

More information on the required arguments is given in the appendix in
Section
[\[Appendix.Statistical\]](#Appendix.Statistical){reference-type="ref"
reference="Appendix.Statistical"}. The discrete quantile functions
`binomial-quant` and `poisson-quant` return values of a left continuous
inverse of the cdf. The pmf's for these distributions are only defined
for integer arguments. The quantile functions and random variable
generators for the beta, gamma, $\chi^{2}$, t and F distributions are
presently calculated by inverting the cdf and may be a bit slow.

-->

The state of the internal random number generator can be "randomly"
reseeded, and the current value of the generator state can be saved. The
mechanism used is the standard Common Lisp mechanism. The current random
state is held in the variable `*random-state*`. The function
`make-random-state` can be used to set and save the state. It takes
an optional argument. If the argument is `NIL` or omitted
`make-random-state` returns a copy of the current value of
`*random-state*`. If the argument is a state object, a copy of it is
returned.  If the argument is `t` a new, "randomly" initialized
state object is produced and returned. [^5]

<!--
### Generating Systematic Data

(defun generate-sequence (result-type size function)
  "Like MAKE-SEQUENCE, but using a function to fill the result."
  (map-into (make-sequence result-type size) function))

alexandria:iota

It might be that generate-sequence can be used with the random-draw
functions for the same effect as the XLISP-STAT (uniform 50). It works with this:

(let ((rv (r-normal 13 2)))
	   (num-utils.utilities:generate-sequence 'vector 10 (lambda () (draw rv))))

But that seems a bit clumsy and (normal 50), or something like it, a bit easier.

NOTE: `iota` doesn't have a range, it has :start and :step, and you have to calculate the range.

We have already used the functions `iseq` and `rseq` to generate equally spaced
sequences of integers and real numbers.  The function `repeat` is
useful for generating sequences with a particular pattern. The general
form of a call to `repeat` is

    (repeat list pattern)

`pattern` must be either a single number or a list of numbers of
the same length as `list`. If `pattern` is a single number
then `repeat` simply repeats `list` `pattern` times. For
example

    > (repeat (list 1 2 3) 2)
    (1 2 3 1 2 3)

If `pattern` is a list then each element of `list` is repeated
the number of times indicated by the corresponding element of
`pattern`. For example

    > (repeat (list 1 2 3) (list 3 2 1))
    (1 1 1 2 2 3)

In Section [6.2](#MorePlots.Scatmat){reference-type="ref"
reference="MorePlots.Scatmat"} below I generate the variables
`density` and `variety` by typing them in directly. Using the
`repeat` function we could have generated them like this:

    (def density (repeat (repeat (list 1 2 3 4) (list 3 3 3 3)) 3))
    (def variety (repeat (list 1 2 3) (list 12 12 12)))
-->
### Forming Subsets and Deleting Cases {#MoreData.Subsets}

The `select` function allows you to select a single element or a
group of elements from a list or vector. For example, if we define
`x` by

```lisp
(def x (list 3 7 5 9 12 3 14 2))
```

then `(select x i)` will return the i<sup>th</sup> element of `x`.
Common Lisp, like the language C, but in contrast to FORTRAN, numbers
elements of list and vectors starting at zero.  Thus the indices for
the elements of `x` are 0, 1, 2, 3, 4, 5, 6, 7 . So

    LS-USER> (select x 0)
    3
    LS-USER> (select x 2)
    5

To get a group of elements at once we can use a list of indices instead
of a single index:

    LS-USER> (select x (list 0 2))
    (3 5)

If you want to select all elements of `x` except element 2 you can
use the expression

```lisp
(remove 2 (iota 8))
```

as the second argument to the function `select`:

```lisp
LS-USER> (remove 2 (iota 8))
(0 1 3 4 5 6 7)
LS-USER> (select x (remove 2 (iota 8)))
(3 7 9 12 3 14 2)
```
<!--
Another approach is to use the logical function `/=` (meaning not
equal) to tell you which indices are not equal to 2. The function
`which` can then be used to return a list of all the indices for
which the elements of its argument are not `NIL`:

    LS-USER> (/= 2 (iota 8))
    (T T NIL T T T T T)
    LS-USER> (which (/= 2 (iota 0 8)))
    (0 1 3 4 5 6 7)
    LS-USER> (select x (which (/= 2 (iseq 0 7))))
    (3 7 9 12 3 14 2)

This approach is a little more cumbersome for deleting a single element,
but it is more general. The expression
`(select x (which (< 3 x)))`, for example, returns all elements in
`x` that are greater than 3:

    LS-USER> (select x (which (< 3 x)))
    (7 5 9 12 14)
-->
### Combining Lists & Vectors

At times you may want to combine several short lists or vectors into a
single longer one.  This can be done using the `append` function. For
example, if you have three variables `x`, `y` and `z` constructed by
the expressions

```lisp
(def x (list 1 2 3))
(def y (list 4))
(def z (list 5 6 7 8))
```

then the expression

```lisp
(append x y z)
```

will return the list

    (1 2 3 4 5 6 7 8).

For vectors, we use the more general function `concatenate`, which
operates on *sequences*, that is objects of either `list` or `vector`:

```lisp
LS-USER> (concatenate 'vector #(1 2) #(3 4))
#(1 2 3 4)
```

Notice that we had to indicate the return type, using the `'vector`
argument to `concatenate`. We could also have said `'list` to have it
return a list, and it would have coerced the arguments to the correct
type.

### Modifying Data

So far when I have asked you to type in a list of numbers I have been
assuming that you will type the list correctly.  If you made an error
you had to retype the entire `def` expression.  Since you can use
cut & paste this is really not too serious.  However it would be
nice to be able to replace the values in a list after you have typed
it in.  The `setf` special form is used for this. Suppose you would
like to change the 12 in the list `x` used in the Section
[4.3](#MoreData.Subsets) to 11. The expression

```lisp
(setf (select x 4) 11)
```

will make this replacement:

```lisp
LS-USER> (setf (select x 4) 11)
11
LS-USER> x
(3 7 5 9 11 3 14 2)
```

The general form of `setf` is

```lisp
(setf form value)
```

where `form` is the expression you would use to select a single
element or a group of elements from `x` and `value` is the
value you would like that element to have, or the list of the values for
the elements in the group. Thus the expression

```lisp
(setf (select x (list 0 2)) (list 15 16))
```

changes the values of elements 0 and 2 to 15 and 16:

    LS-USER> (setf (select x (list 0 2)) (list 15 16))
    (15 16)
    LS-USER> x
    (15 7 16 9 11 3 14 2)

{{< alert color="warning" title="Caution" >}} Lisp symbols are merely labels for
different items. When you assign a name to an item with the `defvar` or `defparameter`
commands you are not producing a new item. Thus

    (defparameter x (list 1 2 3 4))
    (defparameter y x)

means that `x` and `y` are two different names for the same
thing.{{< /alert >}}

As a result, if we change an element of (the item referred to by) `x`
with `setf` then we are also changing the element of (the item
referred to by) `y`, since both `x` and `y` refer to the same item. If
you want to make a copy of `x` and store it in `y` before you make
changes to `x` then you must do so explicitly using, say, the
[copy-list](http://clhs.lisp.se/Body/f_cp_lis.htm) function. The
expression

```lisp
(defparameter y (copy-list x))
```

will make a copy of `x` and set the value of `y` to that copy.
Now `x` and `y` refer to different items and changes to
`x` will not affect `y`.

## Useful Shortcuts {#Shortcuts}

This section describes some additional features of LISP-STAT that you
may find useful.

### Getting Help {#Getting-Help}

On line help is available for many of the functions in LISP-STAT [^6].
As an example, here is how you would get help for the function
`iota`:

```
LS-USER> (documentation 'iota 'function)
"Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consecutive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1.

Examples:

  (iota 4)                      => (0 1 2 3)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)
"
```

Note the quote in front of `iota`. `documentation` is itself a
function, and its argument is the symbol representing the function
`iota`.  To make sure `documentation` receives the symbol, not the
value of the symbol, you need to quote the symbol.

Another useful function is `describe` that, depending on the Lisp
implementation, will return documentation and additional information
about the object:

```
LS-USER> (describe 'iota)
ALEXANDRIA:IOTA
  [symbol]

IOTA names a compiled function:
  Lambda-list: (ALEXANDRIA::N &KEY (ALEXANDRIA::START 0) (STEP 1))
  Derived type: (FUNCTION
                 (UNSIGNED-BYTE &KEY (:START NUMBER) (:STEP NUMBER))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a list of n numbers, starting from START (with numeric contagion
    from STEP applied), each consecutive number being the sum of the previous one
    and STEP. START defaults to 0 and STEP to 1.

    Examples:

      (iota 4)                      => (0 1 2 3)
      (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
      (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)

  Inline proclamation: INLINE (inline expansion available)
  Source file: s:/src/third-party/alexandria/alexandria-1/numbers.lisp
```

{{< alert title="Note" >}}

Generally `describe` is better to use than `documentation`. The ANSI Common Lisp spec
has this to say about documentation:

"Documentation strings are made available for debugging purposes. Conforming programs are permitted to use documentation strings when they are present, but should not depend for their correct behavior on the presence of those documentation strings. An implementation is permitted to discard documentation strings at any time for implementation-defined reasons."

{{< /alert >}}


If you are not sure about the name of a function you may still be able
to get some help. Suppose you want to find out about functions related
to the normal distribution. Most such functions will have "norm" as part
of their name. The expression

    (apropos 'norm)

will print the help information for all symbols whose names contain the
string "norm":

```
ALEXANDRIA::NORMALIZE
ALEXANDRIA::NORMALIZE-AUXILARY
ALEXANDRIA::NORMALIZE-KEYWORD
ALEXANDRIA::NORMALIZE-OPTIONAL
ASDF/PARSE-DEFSYSTEM::NORMALIZE-VERSION (fbound)
ASDF/FORCING:NORMALIZE-FORCED-NOT-SYSTEMS (fbound)
ASDF/FORCING:NORMALIZE-FORCED-SYSTEMS (fbound)
ASDF/SESSION::NORMALIZED-NAMESTRING
ASDF/SESSION:NORMALIZE-NAMESTRING (fbound)
CL-INTERPOL::NORMAL-NAME-CHAR-P (fbound)
CL-PPCRE::NORMALIZE-VAR-LIST (fbound)
DISTRIBUTIONS::+NORMAL-LOG-PDF-CONSTANT+ (bound, DOUBLE-FLOAT)
DISTRIBUTIONS::CDF-NORMAL% (fbound)
DISTRIBUTIONS::COPY-LEFT-TRUNCATED-NORMAL (fbound)
DISTRIBUTIONS::COPY-R-LOG-NORMAL (fbound)
DISTRIBUTIONS::COPY-R-NORMAL (fbound)
DISTRIBUTIONS::DRAW-LEFT-TRUNCATED-STANDARD-NORMAL (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-ALPHA (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-LEFT (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-LEFT-STANDARDIZED (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-M0 (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-MU (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-P (fbound)
DISTRIBUTIONS::LEFT-TRUNCATED-NORMAL-SIGMA (fbound)
DISTRIBUTIONS::MAKE-LEFT-TRUNCATED-NORMAL (fbound)
DISTRIBUTIONS::MAKE-R-LOG-NORMAL (fbound)
DISTRIBUTIONS::MAKE-R-NORMAL (fbound)
DISTRIBUTIONS::QUANTILE-NORMAL% (fbound)
DISTRIBUTIONS::R-LOG-NORMAL-LOG-MEAN (fbound)
...
```
<!-- We need a new version of apropos that does something like this. elisp has it.
    > (apropos 'norm)
    ------------------------------------------------------------------------------
    Sorry, no help available on NORM
    ------------------------------------------------------------------------------
    Sorry, no help available on NORM-2
    ------------------------------------------------------------------------------
    Sorry, no help available on NORMAL
    ------------------------------------------------------------------------------
    NORMAL-CDF                                                      [function-doc]
    Args: (x)
    Returns the value of the standard normal distribution function at X.
    Vectorized.
    ------------------------------------------------------------------------------
    NORMAL-DENS                                                     [function-doc]
    Args: (x)
    Returns the density at X of the standard normal distribution. Vectorized.
    ------------------------------------------------------------------------------
    NORMAL-QUANT                                                    [function-doc]
    Args (p)
    Returns the P-th quantile of the standard normal distribution. Vectorized.
    ------------------------------------------------------------------------------
    NORMAL-RAND                                                     [function-doc]
    Args: (n)
    Returns a list of N standard normal random numbers. Vectorized.
    ------------------------------------------------------------------------------
    NIL

The symbols `norm`, `norm-2` and `normal` do not have
function definitions and thus there is no help available for them. The
term *vectorized* in a function's documentation means the function can
be applied to arguments that are lists or arrays; the result will be a
list or array of the results of applying the function to each element of
its arguments. [^7] A related term appearing in the documentation of
some functions is *vector reducing*. A function is vector reducing if it
is applied recursively to its arguments until a single number results.
The functions `sum`, `prod`, `max` and `min` are
vector reducing.

The first time a help function is used will take some time -- the help
file is scanned and the positions of all entries in the file are
recorded. Subsequent calls will be faster.
-->


Let me briefly explain the notation used in the information printed by
`describe` regarding the arguments a function expects [^8]. This is
called the `lambda-list`. Most functions expect a fixed set of
arguments, described in the help message by a line like `Args: (x y
z)` or `Lambda-list: (x y z)`

Some functions can take one or more optional arguments. The arguments
for such a function might be listed as

    Args: (x &optional y (z t))
or
```
Lambda-list: (x &optional y (z t))
```

This means that `x` is required and `y` and `z` are
optional. If the function is named `f`, it can be called as
`(f x-val)`, `(f x-val y-val)` or
`(f x-val y-val z-val)`. The list `(z t)` means that if
`z` is not supplied its default value is `T`. No explicit
default value is specified for `y`; its default value is therefore
`NIL`. The arguments must be supplied in the order in which they
are listed. Thus if you want to give the argument `z` you must also
give a value for `y`.

Another form of optional argument is the *keyword argument*. The
`iota` function for example takes arguments

    Args: (N &key (START 0) (STEP 1))

The `n` argument is required, the `START` argument is an optional
keyword argument. The default START is 0, and the default STEP
is 1. If you want to create a sequence eight numbers, with a step of
two) use the expression

    (iota 8 :step 2)

Thus to give a value for a keyword argument you give the keyword [^9]
for the argument, a symbol consisting of a colon followed by the
argument name, and then the value for the argument. If a function can
take several keyword arguments then these may be specified in any order,
following the required and optional arguments.

Finally, some functions can take an arbitrary number of arguments. This
is denoted by a line like

    Args: (x &rest args)

The argument `x` is required, and zero or more additional arguments
can be supplied.

In addition to providing information about functions `describe` also
gives information about data types and certain variables. For example,

```
LS-USER> (describe 'complex)
COMMON-LISP:COMPLEX
  [symbol]

COMPLEX names a compiled function:
  Lambda-list: (REALPART &OPTIONAL (IMAGPART 0))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES
                  (OR RATIONAL (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT) (COMPLEX RATIONAL))
                  &OPTIONAL))
  Documentation:
    Return a complex number with the specified real and imaginary components.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

COMPLEX names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:COMPLEX>:
  Class precedence-list: COMPLEX, NUMBER, T
  Direct superclasses: NUMBER
  Direct subclasses: SB-KERNEL:COMPLEX-SINGLE-FLOAT,
                     SB-KERNEL:COMPLEX-DOUBLE-FLOAT
  Sealed.
  No direct slots.

COMPLEX names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (SB-KERNEL::TYPESPEC '*))
```

shows the function, type and class documentation for `complex`, and

    LS-USER> (documentation 'pi 'variable)
    PI                                                              [variable-doc]
    The floating-point number that is approximately equal to the ratio of the
    circumference of a circle to its diameter.


shows the variable documentation for `pi`[^10].

### Listing and Undefining Variables

After you have been working for a while you may want to find out what
variables you have defined (using `def`).  The function
`variables` will produce a listing:

    LS-USER> (variables)
    CO
    HC
    RURAL
    URBAN
    PRECIPITATION
    PURCHASES
    NIL
    LS-USER>

If you are working with very large variables you may occasionally want
to free up some space by getting rid of some variables you no longer
need.  You can do this using the `undef-var` function:

    LS-USER> (undef-var 'co)
    CO
    LS-USER> (variables)
    HC
    RURAL
    URBAN
    PRECIPITATION
    PURCHASES
    NIL
    LS-USER>

### More on the Listener

Common Lisp provides a simple *command history* mechanism. The symbols
`-`, ``, `*`, `**`, `+`, `++`, and `+++` are used for this purpose. The
top level reader binds these symbols as follows:

  ------- -----------------------------------
      `-` the current input expression
      `+` the last expression read
     `++` the previous value of `+`
    `+++` the previous value of `++`
       `` the result of the last evaluation
      `*` the previous value of ``
     `**` the previous value of `*`
  ------- -----------------------------------

The variables ``, `*` and `**` are probably most useful.

For example, if you read a data-frame but forget to assign the
resulting object to a variable:

```lisp
LS-USER> (read-csv rdata:mtcars)
WARNING: Missing column name was filled in
#<DATA-FRAME (32 observations of 12 variables)>
```

you can recover it using one of the history variables:

```lisp
(defparameter mtcars *)
; MTCARS
```

The symbol `MTCARS` now has the data-frame object as its value.

Like most interactive systems, Common Lisp needs a system for
dynamically managing memory.  The system used depends on the
implementation. The most common way (SBCL, CCL) is to grab memory out
of a fixed bin until the bin is exhausted.  At that point the system
pauses to reclaim memory that is no longer being used.  This process,
called *garbage collection*, will occasionally cause the system to
pause if you are using large amounts of memory.


### Loading Files {#Shortcuts.Load}

The data for the examples and exercises in this tutorial, when not
loaded from the network, have been stored on files with names ending
in `.lisp`. In the LISP-STAT system directory they can be found in the
folder `Datasets`. Any variables you save (see the next subsection for
details) will also be saved in files of this form. The data in these
files can be read into LISP-STAT with the `load` function. To load a
file named `randu.lisp` type the expression

```lisp
(load #P"LS:DATASETS;RANDU.LISP")
```

or just

```lisp
(load #P"LS:DATASETS;randu")
```

If you give `load` a name that does not end in `.lisp` then
`load` will add this suffix.

### Saving Your Work

#### Save a Session
If you want to record a session with LISP-STAT you can do so using the
`dribble` function. The expression

```lisp
(dribble "myfile")
```

starts a recording. All expressions typed by you and all results
printed by LISP-STAT will be entered into the file named `myfile`.
The expression

```lisp
(dribble)
```

stops the recording. Note that `(dribble "myfile")` starts a new
file by the name `myfile`. If you already have a file by that name
its contents will be lost. Thus you can't use dribble to toggle on and
off recording to a single file.

`dribble` only records text that is typed, not plots.  However, you
can use the buttons displayed on a plot to save in SVG or PNG format.
The original HTML plots are saved in your operating system's `TEMP`
directory and can be viewed again until the directory is cleared
during a system reboot.

#### Saving Variables
Variables you define in LISP-STAT only exist for the duration of the
current session.  If you quit from LISP-STAT your data will be lost.
To preserve your data you can use the `savevar` function.  This
function allows you to save one a variable into a file.  Again
a new file is created and any existing file by the same name is
destroyed.  To save the variable `precipitation` in a file called
`precipitation` type

```lisp
(savevar 'precipitation "precipitation")
```

Do not add the `.lisp` suffix yourself; `save` will supply
it. To save the two variables `precipitation` and `purchases`
in the file `examples.lisp` type [^11].

```lisp
(savevar '(purchases precipitation) "examples")
```

The files `precipitation.lisp` and `examples.lisp` now contain a set
of expression that, when read in with the `load` command, will
recreate the variables `precipitation` and `purchases`.  You can look
at these files with an editor like the Emacs editor and you can
prepare files with your own data by following these examples.

<!--
Removed until we can cohesively put data frames into the tutorial
#### Saving Data Frames
To save a data frame, use the `save` function. For example to save the
`mpg` data frame you would use:

```lisp
(save mpg #P"LS:DATASETS;mpg.lisp")
```


For more information on saving data frames see the [save section in
the manual](https://lisp-stat.dev/docs/tasks/data-frame/#save-data)
function.
-->

<!-- Describe the CCL Editor, Hemlock, for MacOS users
### The LISP-STAT Editor {#Shortcuts.Editor}

The Macintosh version of XLISP-STAT includes a simple editor for
preparing data files and function definitions. To edit an existing file
select the **Open Edit** item on the **File** menu; to start a new file
select **New Edit**. Other commands on the **File** menu can be used to
save your file and to revert back to the saved version. The editor can
only handle text files of less than 32K characters. As in the listener
window, hitting the *tab* key in any line but the first of a multi-line
expression will indent the expression to a reasonable point. The editor
also allows you to select a section of text representing one or more
Lisp expressions and have these evaluated. To do this select the
expressions you want to evaluate and then choose **Eval Selection** from
the **Edit** menu. The returned values are not available, so this is
only useful for producing side effects, such as defining variables or
functions.
-->

### Reading Data Files

The data files we have used so far in this tutorial have contained
Common Lisp expressions.  LISP-STAT also provides functions for
reading raw data files.  The most commonly used is `read-csv`.

```lisp
(read-csv stream)
```

where `stream` is a Common Lisp stream with the data.  Streams can be
obtained from files, strings or a network and are in _comma separated
value_ (CSV) format.  The parser supports delimiters other than comma.

The character delimited reader should be adequate for most purposes.
If you have to read a file that is not in a character delimited format
you can use the raw file handling functions of Common Lisp.


### User Initialization File

Each Common Lisp implementation provides a way to execute
initialization code upon start-up.  You can use this file to load any
data sets you would like to have available or to define functions of
your own.

LISP-STAT also has an initialization file, `ls-init.lisp`, in your
home directory. Typically you will use the lisp implementation
initialization file for global level initialization, and
`ls-init.lisp` for data related customizations. See the section
[Initialization
file](/docs/getting-started/installation/#initialization-file) in the
manual for more information.

<!-- Document this once Vega-lite/Plotly is working
## More Elaborate Plots {#MorePlots}

The plots described so far were designed for exploring the distribution
of a single variable and the relationship among two variables. This
section describes some plots and techniques that are useful for
exploring the relationship among three or more variables. The techniques
and plots described in the first four subsections are simple, requiring
only simple commands to the listener and mouse actions. The fifth
subsection introduces the concept of a *plot object* and the idea of
*sending messages* to an object from the listener window. These ideas
are used to add lines and curves to scatter plots, and the basic
concepts of objects and messages will be used again in the next section
on regression models. The final subsection shows how Lisp iteration can
be used to construct a dynamic simulation -- a plot movie.

### Spinning Plots {#MorePlots.Spinplot}

If we are interested in exploring the relationship among three variables
then it is natural to imagine constructing a three dimensional
scatterplot of the data. Of course we can only see a two dimensional
projection of the plot on a computer screen -- any depth that you might
be able to perceive by looking at a wire model of the data is lost. One
approach to try to recover some of this depth perception is to rotate
the points around some axis. The `spin-plot` function allows you to
construct a rotatable three dimensional plot.

As an example let's look a some data collected to examine the
relationship between a phosphate absorption index and the amount of
extractable iron and aluminum in a sediment (Devore and Peck
[@DevorePeck page 509, Example 6]). The data can be entered with the
expressions

    (def iron (list 61 175 111 124 130 173 169 169 160 224 257 333 199))
    (def aluminum (list 13 21 24 23 64 38 33 61 39 71 112 88 54)) 
    (def absorption (list 4 18 14 18 26 26 21 30 28 36 65 62 40))

The expression

    (spin-plot (list absorption iron aluminum))

produces the plot on the left in Figure
[\[Spinplot1\]](#Spinplot1){reference-type="ref" reference="Spinplot1"}.

The argument to `spin-plot` is a list of three lists or vectors,
representing the $x$, $y$ and $z$ variables. The $z$ axis is initially
pointing out of the screen. You can rotate the plot by pointing at one
of the `Pitch`, `Roll` or `Yaw` squares and pressing the
mouse button. By rotating the plot you can see that the points seem to
fall close to a plane. The plot on the right of Figure
[\[Spinplot1\]](#Spinplot1){reference-type="ref" reference="Spinplot1"}
shows the data viewed along the plane. A linear model should describe
this data quite well.

As a second example, with the data defined by

    (def strength (list 14.7 48.0 25.6 10.0 16.0 16.8 20.7 38.8
                        16.9 27.0 16.0 24.9 7.3 12.8))
    (def depth (list 8.9 36.6 36.8 6.1 6.9 6.9 7.3 8.4 6.5 8.0 4.5 9.9 2.9 2.0))
    (def water (list 31.5 27.0 25.9 39.1 39.2 38.3 33.9 33.8
                     27.9 33.1 26.3 37.8 34.6 36.4))

(Devore and Peck[@DevorePeck Problem 12.18]) the expression

    (spin-plot (list water depth strength)
               :variable-labels (list "Water" "Depth" "Strength"))

produces a plot that can be rotated to produce the view in Figure
[\[Spinplot2\]](#Spinplot2){reference-type="ref" reference="Spinplot2"}.
These data concern samples of thawed permafrost soil. `strength` is
the shear strength, and `water` is the percentage water content.
`depth` is the depth at which the sample was taken. The plot shows
that a linear model will not fit well. Devore and Peck [@DevorePeck]
suggest fitting a model with quadratic terms to this data.

The function `spin-plot` also accepts the additional keyword
argument `scale`. If `scale` is `T`, the default, then
the data are centered at the mid-ranges of the three variables, and all
three variables are scaled to fit the plot. If `scale` is `NIL` the
data are assumed to be scaled between -1 and 1, and the plot is rotated
about the origin. Thus if you want to center your plot at the means of
the variables and scale all observations by the same amount you can use
the expression

    (spin-plot (list (/ (- water (mean water)) 20)
                     (/ (- depth (mean depth)) 20)
                     (/ (- strength (mean strength)) 20))
               :scale nil)

Note that the `scale` keyword argument is given using the
corresponding keyword symbol, the symbol `scale` preceded by a
colon.

Rotation speed can be changed using the plot menu or the keyboard
equivalents COMMAND-F for Faster and COMMAND-S for Slower.

Depth cuing and showing of the axes are controlled by items on the plot
menu.

If you click the mouse in one of the `Pitch`, `Roll` or
`Yaw` squares while holding down the *shift* key the plot will
start to rotate and continue to rotate after the mouse button has been
released.

### Exercises {#exercises-4 .unnumbered}

1.  An upstate New York business machine company used to include a
    random number generator called RANDU in its software library. This
    generator was supposed to produce numbers that behaved like
    $i. i. d.$ uniform random variables. The data set `randu` in
    the file `randu.lsp` in the **Data** folder consists of a list
    of three lists of numbers. These lists are consecutive triples of
    numbers produced by RANDU. Use `spin-plot` to see if you can
    spot any unusual features in this sample.
-->

<!--
### Scatter-plot Matrices {#MorePlots.Scatmat}

Another approach to graphing a set of variables is to look at a matrix
of all possible pairwise scatter-plots of the variables. The
`scatterplot-matrix` function will produce such a plot. The data

    (def hardness (list 45 55 61 66 71 71 81 86 53 60 64 68 79 81 56
                        68 75 83 88 59 71 80 82 89 51 59 65 74 81 86))
    (def tensile-strength (list 162 233 232 231 231 237 224 219 203 189
                                210 210 196 180 200 173 188 161 119 161
                                151 165 151 128 161 146 148 144 134 127))
    (def abrasion-loss (list 372 206 175 154 136 112 55 45 221 166 164
                             113  82  32 228 196 128 97 64 249 219 186
                             155 114 341 340 284 267 215 148))

were produced in a study of the abrasion loss in rubber tires and the
expression

    (scatterplot-matrix (list hardness tensile-strength abrasion-loss)
                        :variable-labels
                        (list "Hardness" "Tensile Strength" "Abrasion Loss"))

produces the scatterplot matrix in Figure
[\[Scatmat1\]](#Scatmat1){reference-type="ref" reference="Scatmat1"}.

The plot of `abrasion-loss` against `tensile-strength` gives
you an idea of the joint variation in these two variables. But
`hardness` varies from point to point as well. To get an
understanding of the relationship among all three variables it would be
nice to be able to fix `hardness` at various levels and look at the
way the plot of `abrasion-loss` against `tensile-strength`
changes as you change these levels. You can do this kind of exploration
in the scatterplot matrix by using the two highlighting techniques
*selecting* and *brushing*.

-   *Selecting*. Your plot is in the selecting mode when the cursor is
    an arrow. This is the default setting. In this mode you can select a
    point by clicking the mouse on top of it. To select a group of
    points drag a selection rectangle around the group. If the group
    does not fit in a rectangle you can build up your selection by
    holding down the *shift* key as you click or drag. If you click
    without the *shift* key any existing selection will be unselected;
    when the *shift* key is down selected points remain selected.

-   *Brushing*. You can enter the brushing mode by choosing **Mouse
    Mode\...** from the **Scatmat** menu and selecting **Brushing** from
    the dialog box that is popped up. In this mode the cursor will look
    like a paint brush and a dashed rectangle, the *brush*, will be
    attached to your cursor. As you move the brush across the plot
    points in the brush will be highlighted. Points outside of the brush
    will not be highlighted unless they are marked as selected. To
    select points in the brushing mode (make their highlighting
    permanent) hold the mouse button down as you move the brush.

In the plot in Figure [\[Scatmat2\]](#Scatmat2){reference-type="ref"
reference="Scatmat2"}

the points within the middle of the `hardness` range have been
highlighted using a long, thin brush (you can change the size of your
brush using the **Resize Brush** command on the **Scatmat** menu). In
the plot of `abrasion-loss` against `tensile-strength` you can
see that the highlighted points seem to follow a curve. If you want to
fit a model to this data this suggests fitting a model that accounts for
this curvature.

A scatterplot matrix is also useful for examining the relationship
between a quantitative variable and several categorical variables. In
the data

    (def yield (list 7.9 9.2 10.5 11.2 12.8 13.3 12.1 12.6 14.0 9.1 10.8 12.5
                     8.1 8.6 10.1 11.5 12.7 13.7 13.7 14.4 15.5 11.3 12.5 14.5  
                    15.3 16.1 17.5 16.6 18.5 19.2 18.0 20.8 21 17.2 18.4 18.9 )) 
    (def density (list 1 1 1 2 2 2 3 3 3 4 4 4 1 1 1 2 2 2 3 3 3 4 4 4 
                       1 1 1 2 2 2 3 3 3 4 4 4))
    (def variety (list 1 1 1  1 1 1  1 1 1  1 1 1  2 2 2  2 2 2  2 2 2
                       2 2 2  3 3 3  3 3 3  3 3 3  3 3 3))

(Devore and Peck [@DevorePeck page 595, Example 14]) the yield of tomato
plants is recorded for an experiment run at four different planting
densities and using three different varieties. In the plot in Figure
[\[Scatmat3\]](#Scatmat3){reference-type="ref" reference="Scatmat3"}

a long, thin brush has been used to highlight the points in the third
variety. If there is no interaction between the varieties and the
density then the shape of the highlighted points should move
approximately in parallel as the brush is moved from one variety to
another.

Like `spin-plot`, the function `scatterplot-matrix` also
accepts the optional keyword argument `scale`.


### Exercises {#exercises-5 .unnumbered}

1.  Devore and Peck [@DevorePeck Exercise 13.62] describe an experiment
    to determine the effect of oxygen concentration on fermentation end
    products. Four oxygen concentrations and two types of sugar were
    used. The data are

        (def ethanol (list .59 .30 .25 .03 .44 .18 .13 .02 .22 .23 .07
                           .00 .12 .13 .00 .01))
        (def oxygen (list 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4))
        (def sugar (list 1 1 2 2 1 1 2 2 1 1 2 2 1 1 2 2 ))

    and are on file the `oxygen.lsp`. Use a scatterplot matrix to
    examine these data.

2.  Use scatterplot matrices to examine the data in the examples and
    exercises of Section [6.1](#MorePlots.Spinplot){reference-type="ref"
    reference="MorePlots.Spinplot"} above.

-->

<!--

### Interacting with Individual Plots

Rotating plots and scatterplot matrices are interactive plots. Simple
scatter plots also allow some interaction: If you select the **Show
Labels** option in the plot menu a label will appear next to a
highlighted point. You can use either the selecting or the brushing mode
to highlight points. The default labels are of the form "0", "1", ... .
(In Lisp it is conventional to start numbering indices with 0, rather
than 1.) Most plotting functions allow you to supply a list of case
labels using the `:point-labels` keyword.

Another option, useful in viewing large data sets, is to remove a subset
of the points from your plot. This can be done by selecting the points
you want to remove and then choosing **Remove Selection** from the plot
menu. The plot can then be rescaled using the **Rescale Plot** option.
Alternatively, the **Focus on Selection** menu item removes all
unselected points from the plot.

When a set of points is selected in a plot you can change the symbol
used to display the points using the **Selection Symbol** item. On
systems with color monitors you can set the color of selected points
with the **Selection Color** item.

You can save the indices of the selected points in a variable by
choosing the **Selection\...** item in the plot menu. A dialog will ask
you for a name for the selection. When no points are selected you can
use the **Selection\...** menu item to specify the indices of the points
to select. A dialog will ask you for an expression for determining the
selection to use. The expression can be any Lisp expression that
evaluates to a list of indices.

### Linked Plots

When you brush or select in a scatterplot matrix you are looking at the
interaction of a set of separate scatter-plots[^12]. You can construct
your own set of interacting plots by choosing the **Link View** option
from the menus of the plots you want to link. For example, using the
data from Exercise 1 in Section
[6.2](#MorePlots.Scatmat){reference-type="ref"
reference="MorePlots.Scatmat"} we can put `ethanol` and
`oxygen` in a scatterplot and `sugar` in a histogram. If we
link these two plots then selecting one of the two sugar groups in the
histogram highlights the corresponding points in the scatterplot, as
shown in Figure
[\[ScatterplotHist\]](#ScatterplotHist){reference-type="ref"
reference="ScatterplotHist"}.

If you want to be able to select the points with particular labels you
can use the `name-list` function to generate a window with a list
of names in it. This window can be linked with any plot, and selecting a
name in a name list will then highlight the corresponding points in the
linked plots. You can use the `name-list` function with a numerical
argument; e. g.

    (name-list 10)

will generate a list with the names "0" , ..., "9", or you can give it a
list of case labels of your own.

### Exercise {#exercise .unnumbered}

Try to use linked scatter plots and histograms on the data in the
examples and exercises of Sections
[6.1](#MorePlots.Spinplot){reference-type="ref"
reference="MorePlots.Spinplot"} and
[6.2](#MorePlots.Scatmat){reference-type="ref"
reference="MorePlots.Scatmat"}.

### Modifying a Scatter Plot {#MorePlots.Modifying}

After producing a scatterplot of a data set we might like to add a line,
a regression line for example, to the plot. As an example, Devore and
Peck [@DevorePeck page 105, Example 2] describe a data set collected to
examine the effect of bicycle lanes on drivers and bicyclists. The
variables given by

    (def travel-space (list 12.8 12.9 12.9 13.6 14.5 14.6 15.1 17.5 19.5 20.8))
    (def separation (list 5.5 6.2 6.3 7.0 7.8 8.3 7.1 10.0 10.8 11.0))

represent the distance between the cyclist and the roadway center line
and the distance between the cyclist and a passing car, respectively,
recorded in ten cases. A regression line fit to these data, with
`separation` as the dependent variable, has a slope of 0.66 and an
intercept of -2.18. Let's see how to add this line to a scatterplot of
the data.

We can use the expression

    (plot-points travel-space separation)

to produce a scatterplot of these points. To be able to add a line to
the plot, however, we must be able to refer to it within XLISP-STAT. To
accomplish this let's assign the result returned by the
`plot-points` function to a symbol [^13]:

    (def myplot (plot-points travel-space separation))

The result returned by plot-points is an XLISP-STAT *object* . To use an
object you have to send it *messages*. This is done using the
`send` function, as in the expression

    (send object message argument1 ... )

I will use the expression

    (send myplot :abline -2.18 0.66)

to tell `myplot` to add the graph of a line $a + b x$, with $a =
-2.18$ and $b = 0.66$, to itself. The *message selector* is
`:abline`, the numbers -2.18 and 0.66 are the arguments. The
message consists of the selector and the arguments. Message selectors
are always Lisp keywords; that is, they are symbols that start with a
colon. Before typing in this expression you might want to resize and
rearrange the listener window so you can see the plot and type commands
at the same time. Once you have resized the listener window you can
easily switch between the small window and a full size window by
clicking in the zoom box at the right corner of the window title bar.
The resulting plot is shown in Figure
[\[Scatterplot4\]](#Scatterplot4){reference-type="ref"
reference="Scatterplot4"}

Scatter plot objects understand a number of other messages. One other
message is the `:help` message [^14]:

    > (send myplot :help)
    > (send scatterplot-proto :help)
    SCATTERPLOT-PROTO
    Scatterplot.
    Help is available on the following:

    :ABLINE :ACTIVATE :ADD-FUNCTION :ADD-LINES :ADD-METHOD :ADD-MOUSE-MODE
    :ADD-POINTS :ADD-SLOT :ADD-STRINGS :ADJUST-HILITE-STATE
    :ADJUST-POINT-SCREEN-STATES :ADJUST-POINTS-IN-RECT :ADJUST-TO-DATA
    :ALL-POINTS-SHOWING-P :ALL-POINTS-UNMASKED-P :ALLOCATE
    :ANY-POINTS-SELECTED-P :APPLY-TRANSFORMATION :BACK-COLOR :BRUSH
    :BUFFER-TO-SCREEN :CANVAS-HEIGHT :CANVAS-WIDTH :CLEAR :CLEAR-LINES
    :CLEAR-MASKS :CLEAR-POINTS :CLEAR-STRINGS ....

The list of topics will be the same for all scatter plots but will be
somewhat different for rotating plots, scatterplot matrices or
histograms.

The `:clear` message, as its name suggests, clears the plot and
allows you to build up a new plot from scratch. Two other useful
messages are `:add-points` and `:add-lines`. To find out how
to use them we can use the `:help` message with `:add-points`
or `:add-lines` as arguments:

    > (send myplot :help :add-points)
    :ADD-POINTS
    Method args: (points &key point-labels (draw t))
    Adds points to plot. POINTS is a list of sequences, POINT-LABELS a list of
    strings. If DRAW is true the new points are added to the screen.
    NIL
    > (send myplot :help :add-lines)
    :ADD-LINES
    Method args: (lines &key type (draw t))
    Adds lines to plot. LINES is a list of sequences, the coordinates of the line 
    starts. TYPE is normal or dashed. If DRAW is true the new lines are added to the
    screen.
    NIL
    > 

The plot produced above shows some curvature in the data. A regression
of `separation` on a linear and a quadratic term in
`travel-space` produces estimates of -16.41924 for the constant,
2.432667 as the coefficient of the linear term and -0.05339121 as the
coefficient of the quadratic term. Let's use the `:clear`,
`:add-points` and `:add-lines` messages to change
`myplot` to show the data along with the fitted quadratic model.
First we use the expressions

    (def x (rseq 12 22 50))
    (def y (+ -16.41924 (* 2.432667 x) (* -0.05339121 (* x x))))

to define `x` as a grid of 50 equally spaced points between 12 and
22 and `y` as the fitted response at these `x` values. Then
the expressions

    (send myplot :clear)
    (send myplot :add-points travel-space separation)
    (send myplot :add-lines x y)

change myplot to look like Figure
[\[Scatterplot5\]](#Scatterplot5){reference-type="ref"
reference="Scatterplot5"}.

Of course we could have used `plot-points` to get a new plot and
just modified that plot with `:add-lines`, but the approach used
here allowed us to try out all three messages.

### Dynamic Simulations {#MorePlots.Dynamic}

As another illustration of what you can do by modifying existing plots
let's construct a dynamic simulation -- a movie -- to examine the
variation in the shape of histograms of samples from a standard normal
distribution. To start off, use the expression

    (def h (histogram (normal-rand 20)))

to construct a single histogram and save its plot object as `h`.
The `:clear` message is available for histograms as well. As you
can see from its help message

    > (send h :help :clear)
    :CLEAR

    Message args: (&key (draw t))
    Clears the plot data. If DRAW is nil the plot is redrawn; otherwise its
    current screen image remains unchanged.
    NIL
    >

the `:clear` message takes an optional keyword argument. If this
argument is `NIL` then the plot will not actually be redrawn until some
other event causes it to be redrawn. This is useful for dynamic
simulations. Rearrange and resize your windows until you can see the
histogram window even when the listener window is the active window.
Then type the expression [^15]

    (dotimes (i 50)
             (send h :clear :draw nil)
             (send h :add-points (normal-rand 20)))

This should produce a sequence of 50 histograms, each based on a sample
of size 20. By giving the keyword argument `draw` with value
`NIL` to the `:clear` message you have insured that each histogram
stays on the screen until the next one is ready to be drawn. Try the
example again without this argument and see what difference it makes.

Programmed dynamic simulations may provide another approach to viewing
the relation among several variables. As a simple example, suppose we
want to examine the relation between the variables `abrasion-loss`
and `hardness` introduced in Section
[6.2](#MorePlots.Scatmat){reference-type="ref"
reference="MorePlots.Scatmat"} above. Let's start with a histogram of
`abrasion-loss` produced by the expression

    (def h (histogram abrasion-loss))

The messages `:point-selected` , `:point-hilited` and
`:point-showing` are particularly useful for dynamic simulations.
Here is the help information for `:point-selected` in a histogram:

    > (send h :help :point-selected)
    :POINT-SELECTED
    Method args: (point &optional selected)
    Sets or returns selection status (true or NIL) of POINT. Sends
    :ADJUST-POINT-SCREEN-STATES message if states are set. Vectorized.
    NIL
    >

Thus you can use this message to determine whether a point is currently
selected and also to select or unselect it. Again rearrange the windows
so you can see the histogram while typing in the listener window and
type the expression

    (dolist (i (order hardness))
            (send h :point-selected i t)
            (send h :point-selected i nil))

The expression `(order hardness)` produces the list of indices of
the ordered values of hardness. Thus the first element of the result is
the index of the smallest element of hardness, the second element is the
index of the second smallest element of hardness, etc.. The loop moves
through each of these indices and selects and unselects the
corresponding point.

The result on the screen is very similar to the result of brushing a
`hardness` histogram linked to an `abrasion-loss` histogram
from left to right. The drawback of this approach is that it is harder
to write an expression than to use a mouse. On the other hand, when
brushing with a mouse you tend to focus your attention on the plot you
are brushing, rather than on the other linked plots. When you run a
dynamic simulation you do not have to do anything while the simulation
is running and can therefore concentrate fully on the results.

An intermediate solution is possible: You can set up a dialog window
with a scroll bar that will run through the indices in the list
`(order hardness)`, selecting the corresponding point as it is
scrolled. An example in Section [8](#Fundefs){reference-type="ref"
reference="Fundefs"} will show you how to do this.

Like most Lisp systems XLISP-STAT is not ideally suited to real time
simulations because of the need for garbage collection, to reclaim
dynamically allocated storage. This is the reason that the simulations
in this section will occasionally pause for a second or two.
Nevertheless, in a simple simulation like the last one each iteration
may still be too fast for you to be able to pick up any pattern. To slow
things down you can add some extra busy work to each iteration. For
example, you could use

    (dolist (i (order hardness))
            (send h :point-selected i t)
            (dotimes (i 100))
            (send h :point-selected i nil))

in place of the previous expression.

-->

<!--

## Regression {#Regression}

Regression models have been implemented using the Common Lisp Object
System (CLOS). These were introduced above in Section
[6.5](#MorePlots.Modifying){reference-type="ref"
reference="MorePlots.Modifying"}. You might want to review that
section briefly before reading on.

Let's fit a simple regression model to the bicycle data of Section
[6.5](#MorePlots.Modifying){reference-type="ref"
reference="MorePlots.Modifying"}. The dependent variable is
`separation` and the independent variable is `travel-space`.
To form a regression model use the `regression-model` function:

    > (regression-model travel-space separation)

    Least Squares Estimates:

    Constant               -2.182472   (1.056688)
    Variable 0             0.6603419   (0.06747931)

    R Squared:              0.922901
    Sigma hat:             0.5821083
    Number of cases:              10
    Degrees of freedom:            8

    #<Object: 1966006, prototype = REGRESSION-MODEL-PROTO>
    >

The basic syntax for the `regression-model` function is

    (regression-model x y)

For a simple regression `x` can be a single list or vector. For a
multiple regression `x` can be a list of lists or vectors or a
matrix. The `regression-model` function also takes several optional
keyword arguments. The most important ones are `:intercept`,
`:print`, and `:weights`. Both `:intercept` and
`:print` are `T` by default. To get a model without an
intercept use the expression

    (regression-model x y :intercept nil)

To form a weighted regression model use the expression

    (regression-model x y :weights w)

where `w` is a list or vector of weights the same length as
`y`. In a weighted model the variances of the errors are assumed to
be inversely proportional to the weights `w`.

The `regression-model` function prints a very simple summary of the
fit model and returns a model object as its result. To be able to
examine the model further assign the returned model object to a variable
using an expression like [^16]

    (def bikes (regression-model travel-space separation :print nil))

I have given the keyword argument `:print nil` to suppress the
printing of the summary, since we have already seen it. To find out what
messages are available use the `:help` message:

    > (send bikes :help)
    REGRESSION-MODEL-PROTO
    Normal Linear Regression Model
    Help is available on the following:

    :ADD-METHOD :ADD-SLOT :BASIS :CASE-LABELS :COEF-ESTIMATES :COEF-STANDARD-ERRORS
    :COMPUTE :COOKS-DISTANCES :DELETE-METHOD :DELETE-SLOT :DF :DISPLAY :DOC-TOPICS
    :DOCUMENTATION :EXTERNALLY-STUDENTIZED-RESIDUALS :FIT-VALUES :GET-METHOD
    :HAS-METHOD :HAS-SLOT :HELP :INCLUDED :INTERCEPT :INTERNAL-DOC :ISNEW
    :LEVERAGES :METHOD-SELECTORS :NEW :NUM-CASES :NUM-COEFS :NUM-INCLUDED
    :OWN-METHODS :OWN-SLOTS :PARENTS :PLOT-BAYES-RESIDUALS :PLOT-RESIDUALS
    :PRECEDENCE-LIST :PREDICTOR-NAMES :PRINT :R-SQUARED :RAW-RESIDUALS
    :RESIDUAL-SUM-OF-SQUARES :RESIDUALS :RESPONSE-NAME :RETYPE :SAVE :SHOW
    :SIGMA-HAT :SLOT-NAMES :SLOT-VALUE :STUDENTIZED-RESIDUALS :SUM-OF-SQUARES
    :SWEEP-MATRIX :TOTAL-SUM-OF-SQUARES :WEIGHTS :X :X-MATRIX :XTXINV :Y PROTO
    NIL
    >

Many of these messages are self explanatory, and many have already been
used by the `:display` message, which `regression-model` sends
to the new model to print the summary. As examples let's try the
`:coef-estimates` and `:coef-standard-errors` messages [^17]:

    > (send bikes :coef-estimates)
    (-2.182472 0.6603419)
    > (send bikes :coef-standard-errors)
    (1.056688 0.06747931)
    >

The `:plot-residuals` message will produce a residual plot. To find
out what residuals are plotted against let's look at the help
information:

    > (send bikes :help :plot-residuals)
    :PLOT-RESIDUALS

    Message args: (&optional x-values)
    Opens a window with a plot of the residuals. If X-VALUES are not supplied
    the fitted values are used. The plot can be linked to other plots with the
    link-views function. Returns a plot object.
    NIL
    >

Using the expressions

        (plot-points travel-space separation)
        (send bikes :plot-residuals travel-space)

we can construct two plots of the data as shown in Figure
[\[Scatterplot6\]](#Scatterplot6){reference-type="ref"
reference="Scatterplot6"}. By linking the plots we can use the mouse to
identify points in both plots simultaneously. A point that stands out is
observation 6 (starting the count at 0, as usual).

The plots both suggest that there is some curvature in the data; this
curvature is particularly pronounced in the residual plot if you ignore
observation 6 for the moment. To allow for this curvature we might try
to fit a model with a quadratic term in `travel-space`:

    > (def bikes2  (regression-model (list travel-space (^ travel-space 2))
                                     separation))

    Least Squares Estimates:

    Constant               -16.41924   (7.848271)
    Variable 0              2.432667   (0.9719628)
    Variable 1           -0.05339121   (0.02922567)

    R Squared:             0.9477923
    Sigma hat:             0.5120859
    Number of cases:              10
    Degrees of freedom:            7

    BIKES2
    >

I have used the exponentiation function "`^`" to compute the square of
`travel-space`. Since I am now forming a multiple regression model
the first argument to `regression-model` is a list of the `x`
variables.

You can proceed in many directions from this point. If you want to
calculate Cook's distances for the observations you can first compute
internally studentized residuals as

    (def studres (/ (send bikes2 :residuals)
                    (* (send bikes2 :sigma-hat)
                       (sqrt (- 1 (send bikes2 :leverages))))))

Then Cook's distances are obtained as [^18]

    > (* (^ studres 2)
         (/ (send bikes2 :leverages) (- 1 (send bikes2 :leverages)) 3))
    (0.166673 0.00918596 0.03026801 0.01109897 0.009584418 0.1206654 0.581929
    0.0460179 0.006404474 0.09400811)

The seventh entry -- observation 6, counting from zero -- clearly stands
out.

Another approach to examining residuals for possible outliers is to use
the Bayesian residual plot proposed by Chaloner and Brant
[@ChalonerBrant], which can be obtained using the message
`:plot-bayes-residuals`. The expression
`(send bikes2 :plot-bayes-residuals)` produces the plot in Figure
[\[Scatterplot7\]](#Scatterplot7){reference-type="ref"
reference="Scatterplot7"}.

The bars represent mean $\pm 2SD$ of the posterior distribution of the
actual realized errors, based on an improper uniform prior distribution
on the regression coefficients. The $y$ axis is in units of
$\hat{\sigma}$. Thus this plot suggests the probability that point 6 is
three or more standard deviations from the mean is about 3%; the
probability that it is at least two standard deviations from the mean is
around 50%.

Several other methods are available for residual and case analysis.
These include `:studentized-residuals` and `:cooks-distances`,
which we could have used above instead of calculating these quantities
from their definitions. Another useful message is `:included`,
which can be used to change the cases to be used in estimating a model.
Further details on these messages are available in their help
information.

### Exercises {#exercises-6 .unnumbered}

1.  Using the variables `absorption`, `iron` and
    `aluminum` introduced in Section
    [6.1](#MorePlots.Spinplot){reference-type="ref"
    reference="MorePlots.Spinplot"} above construct and examine a model
    with `absorption` as the dependent variable.

2.  Using the variables `abrasion-loss`, `tensile-strength`
    and `hardness` introduced in Section
    [6.2](#MorePlots.Scatmat){reference-type="ref"
    reference="MorePlots.Scatmat"} above construct and examine a model
    with `abrasion-loss` as the dependent variable.

-->

## Defining Functions & Methods {#Fundefs}

This section gives a brief introduction to programming LISP-STAT. The
most basic programming operation is to define a new function. Closely
related is the idea of defining a new method for an object. [^19]

### Defining Functions

You can use the Common Lisp language to define functions of your
own. Many of the functions you have been using so far are written in
this language.  The special form used for defining functions is called
`defun`. The simplest form of the `defun` syntax is

    (defun fun args expression)

where `fun` is the symbol you want to use as the function name, `args`
is the list of the symbols you want to use as arguments, and
`expression` is the body of the function.  Suppose for example that
you want to define a function to delete a case from a list.  This
function should take as its arguments the list and the index of the
case you want to delete.  The body of the function can be based on
either of the two approaches described in Section
[4.3](#MoreData.Subsets) above. Here is one approach:

```lisp
(defun delete-case (x i)
  (select x (remove i (iota (- (length x) 1)))))
```
<!-- This example would be better written in terms of precipitation -->

I have used the function `length` in this definition to determine
the length of the argument `x`.  Note that none of the arguments to
`defun` are quoted: `defun` is a special form that does not
evaluate its arguments.

Unless the functions you define are very simple you will probably want
to define them in a file and load the file into LISP-STAT with the
`load` command. You can put the functions in the implementation's initialization
file or include in the initialization file a `load`
command that will load another file.  The version of Common Lisp for the
Macintosh, CCL, includes a simple editor that can be used from within
LISP-STAT.

<!--
You can also write functions that work with objects.  Here is a
function that takes two regression models, assumed to be nested, and
computes the $F$ statistic for comparing these models:

    (defun f-statistic (m1 m2)
    "Args: (m1 m2)
    Computes the F statistic for testing model m1 within model m2."
      (let ((send ss1 (m1 :sum-of-squares))
            (send df1 (m1 :df))
            (send ss2 (m2 :sum-of-squares))
            (send df2 (m2 :df)))
        (/ (/ (- ss1 ss2) (- df1 df2)) (/ ss2 df2))))

This definition uses the Lisp `let` construct to establish some
local *variable bindings*. The variables `ss1`, `df1`,
`ss2` and `df2` are defined in terms of the two model
arguments `m1` and `m2`, and are then used to compute the $F$
statistic. The string following the argument list is a *documentation
string*. When a documentation string is present in a `defun`
expression `defun` will install it so the `documentation` function will
be able to retrieve it.
-->

<!--
### Anonymous Functions {#Fundefs.Anonymous}

Suppose you would like to plot the function f(x) = 2x + x^{2} over the
range -2 <= x <= 3.  We can do this by first defining a function
`f` and then using `plot-function`:

    (defun f (x) (+ (* 2 x) (^ x 2)))
    (plot-function #'f -2 3)

This is not too hard, but it nevertheless involves one unnecessary step:
naming the function `f`.  You probably won't need this function
again; it is a throw-away function defined only to allow you to give it
to `plot-function` as an argument.  It would be nice if you could
just give `plot-function` an expression that constructs the
function you want.  Here is such an expression:

    (function (lambda (x) (+ (* 2 x) (^ x 2))))

There are two steps involved. The first is to specify the definition of
your function. This is done using a *lambda expression*, in this case

    (lambda (x) (+ (* 2 x) (^ x 2)))

A lambda expression is a list starting with the symbol `lambda`,
followed by the list of arguments and the expressions making up the body
of the function. The lambda expression is only a definition, it is not
yet a function, a piece of code that can be applied to arguments. The
special form `function` takes the lambda list and constructs such a
function. The result can be saved in a variable or it can be passed on
to another function as an argument. For our plotting problem you can use
the single expression

    (plot-function (function (lambda (x) (+ (* 2 x) (^ x 2)))) -2 3)

or, using the `#’` short form,

    (plot-function #'(lambda (x) (+ (* 2 x) (^ x 2))) -2 3)

Since the function used in these expressions is never named it is
sometimes called an *anonymous function*.

You can also construct a rotating plot of a function of two variables
using the function `spin-function`. As an example, the expression

    (spin-function #'(lambda (x y) (+ (^ x 2) (^ y 2))) -1 1 -1 1)

constructs a plot of the function $f(x, y) = x^{2} + y^{2}$ over the
range $-1 \leq x \leq 1, -1 \leq y \leq 1$ using a $6 \times 6$ grid.
The number of grid points can be changed using the `:num-points`
keyword. The result is shown in Figure
[\[Spinplot3\]](#Spinplot3){reference-type="ref" reference="Spinplot3"}.
Again it convenient to use a lambda expression to specify the function
to be plotted.

There are a number of other situations in which you might want to pass a
function on as an argument without first going through the trouble of
thinking up a name for the function and defining it using `defun`.
A few additional examples are given in the next subsection.
-->

<!--
### Some Dynamic Simulations

In Section [6.6](#MorePlots.Dynamic){reference-type="ref"
reference="MorePlots.Dynamic"} we used a loop to control a dynamic
simulation in which points in a histogram of one variable were selected
and deselected in the order of a second variable. Let's look at how to
run the same simulation using a *slider* to control the simulation.

A slider is a modeless dialog box containing a scroll bar and a value
display field. As the scroll bar is moved the displayed value is changed
and an action is taken. The action is defined by an *action function*
given to the scroll bar, a function that is called with one value, the
current slider value, each time the value is changed by the user. There
are two kinds of sliders, sequence sliders and interval sliders.
Sequence sliders take a sequence (a list or a vector) and scroll up and
down the sequence. The displayed value is either the current sequence
element or the corresponding element of a display sequence. An interval
slider dialog takes an interval, divides it into a grid and scrolls
through the grid. By default a grid of around 30 points is used; the
exact number and the interval end points are adjusted to give nice
printed values. The current interval point is displayed in the display
field.

For our example let's use a sequence slider to scroll through the
elements of the `hardness` list in order and select the
corresponding element of `abrasion-loss`. The expression

    (def h (histogram abrasion-loss))

sets up a histogram and saves its plot object in the variable `h`.
The function `sequence-slider-dialog` takes a list or vector and
opens a sequence slider to scroll through its argument. To do something
useful with this dialog we need to give it an action function as the
value of the keyword argument `:action`. The function should take
one argument, the current value of the sequence controlled by the
slider. The expression

    (sequence-slider-dialog (order hardness) :action
                            #'(lambda (i)
                                (send h :unselect-all-points)
                                (send h :point-selected i t)))

sets up a slider for moving the selected point in the
`abrasion-loss` histogram along the order of the `hardness`
variable. The histogram and scroll bar are shown in Figure
[\[Dynaplot1\]](#Dynaplot1){reference-type="ref" reference="Dynaplot1"}.

The action function is called every time the slider is moved. It is
called with the current element of the sequence `(order hardness)`,
the index of the point to select. It clears all current selections and
then selects the point specified in the call from the slider. The body
of the function is almost identical to the body of the `dotimes`
loop used in Section [6.6](#MorePlots.Dynamic){reference-type="ref"
reference="MorePlots.Dynamic"}. The slider is thus an interactive,
graphically controlled version of this loop.

As another example, suppose we would like to examine the effect of
changing the exponent in a Box-Cox power transformation $$h(x) = \left\{
\begin{array}{cl}
\frac{\textstyle x^{\lambda} - 1}{\textstyle \lambda}
& \mbox{if $\lambda \not= 0$}\\
\\
\log(x)
& \mbox{otherwise}
\end{array}
\right.$$ on a probability plot. As a first step we might define a
function to compute the power transformation and normalize the result to
fall between zero and one:

    (defun bc (x p)
      (let* ((bcx (if (< (abs p) .0001) (log x) (/ (^ x p) p)))
             (min (min bcx))
             (max (max bcx)))
        (/ (- bcx min) (- max min))))

This definition uses the `let*` form to establish some local
variable bindings. The `let*` form is like the `let` form used
above except that `let*` defines its variables sequentially,
allowing a variable to be defined in terms of other variables already
defined in the `let*` expression; `let` on the other hand
creates its assignments in parallel. In this case the variables
`min` and `max` are defined in terms of the variable
`bcx`.

Next we need a set of positive numbers to transform. Let's use a sample
of thirty observations from a $\chi^{2}_{4}$ distribution and order the
observations:

    (def x (sort-data (chisq-rand 30 4)))

The normal quantiles of the expected uniform order statistics are given
by

    (def r (normal-quant (/ (iseq 1 30) 31)))

and a probability plot of the untransformed data is constructed using

    (def myplot (plot-points r (bc x 1)))

Since the power used is 1 the function `bc` just rescales the data.

There are several ways to set up a slider dialog to control the power
parameter. The simplest approach is to use the function
`interval-slider-dialog`:

    (interval-slider-dialog (list -1 2)
                            :points 20
                            :action #'(lambda (p)
                                       (send myplot :clear nil)
                                       (send myplot :add-points r (bc x p))))

`interval-slider-dialog` takes a list of two numbers, the lower and
upper bounds of an interval. The action function is called with the
current position in the interval.

This approach works fine on a Mac II but may be a bit slow on a Mac Plus
or a Mac SE. An alternative is to pre-compute the transformations for a
list of powers and then use the pre-computed values in the display. For
example, using the powers defined in

    (def powers (rseq -1 2 16))

we can compute the transformed data for each power and save the result
as the variable `xlist`:

    (def xlist (mapcar #'(lambda (p) (bc x p)) powers))

The function `mapcar` is one of several *mapping functions*
available in Lisp. The first argument to `mapcar` is a function.
The second argument is a list. `mapcar` takes the function, applies
it to each element of the list and returns the list of the results
[^20]. Now we can use a sequence slider to move up and down the
transformed values in `xlist`:

    (sequence-slider-dialog xlist
                            :display powers
                            :action #'(lambda (x)
                                       (send myplot :clear nil)
                                       (send myplot :add-points r x)))

Note that we are scrolling through a list of lists and the element
passed to the action function is thus the list of current transformed
values. We would not want to see these values in the display field on
the slider, so I have used the keyword argument `:display` to
specify an alternative display sequence, the powers used in the
transformation.
-->

<!--
### Defining Methods

When a message is sent to an object the object system will use the
object and the method selector to find the appropriate piece of code to
execute. Different objects may thus respond differently to the same
message. A linear regression model and a nonlinear regression model
might both respond to a `:coef-estimates` message but they will
execute different code to compute their responses.

The code used by an object to respond to a message is called a *method*.
Objects are organized in a hierarchy in which objects *inherit* from
other objects. If an object does not have a method of its own for
responding to a message it will use a method inherited from one of its
ancestors. The `send` function will move up the *precedence list*
of an object's ancestors until a method for a message is found.

Most of the objects encountered so far inherit directly from *prototype*
objects. Scatter-plots inherit from `scatterplot-proto`, histograms
from `histogram-proto`, regression models from
`regression-model-proto`. Prototypes are just like any other
objects. They are essentially *typical* versions of a certain kind of
object that define default behavior. Almost all methods are owned by
prototypes. But any object can own a method, and in the process of
debugging a new method it is often better to attach the method to a
separate object constructed for that purpose instead of the prototype.

To add a method to an object you can use the `defmeth` macro. As an
example, in Section [7](#Regression){reference-type="ref"
reference="Regression"} we calculated Cook's distances for a regression
model. If you find that you are doing this very frequently then you
might want to define a `:cooks-distances` method. The general form
of a method definition is:

    (defmeth object :new-method arg-list body)

`object` is the object that is to own the new method. In the case
of regression models this can be either a specific regression model or
the prototype that all regression models inherit from,
`regression-model-proto`. The argument `:new-method` is the
message selector for your method; in our case this would be
`:cooks-distances`. The argument `arg-list` is the list of
argument names for your method, and `body` is one or more
expressions making up the body of the method. When the method is used
each of these expressions will be evaluated, in the order in which they
appear.

Here is an expression that will install the `:cooks-distances`
method:

    (defmeth regression-model-proto :cooks-distances ()
    "Message args: ()
    Returns Cooks distances for the model."
      (let* ((leverages (send self :leverages))
             (studres (/ (send self :residuals)
                         (* (send self :sigma-hat) (sqrt (- 1 leverages)))))
             (num-coefs (send self :num-coefs)))
        (* (^ studres 2) (/ leverages (- 1 leverages) num-coefs)))))

The variable `self` refers to the object that is receiving the
message. This definition is close to the definition of this method
supplied in the file `regression.lsp`.



### Plot Methods

All plot activities are accomplished by sending messages to plot
objects. For example, every time a plot needs to be redrawn the system
sends the plot the `:redraw` message. By defining a new method for
this message you can change the way a plot is drawn. Similarly, when the
mouse is moved or clicked in a plot the plot is sent the
`:do-mouse` message. Menu items also send messages to their plots
when they are selected. If you are interested in modifying plot behavior
you may be able to get started by looking at the methods defined in the
graphics files loaded on start up. Further details will be given in
[@MyBook].
-->


## Matrices and Arrays {#Arrays}

LISP-STAT includes support for multidimensional arrays. In addition to
the standard Common Lisp array functions, LISP-STAT also includes a
system called
[array-operations](/docs/tasks/array-operations/).

<!-- document the linear algebra functions, if any, in alexandria or numerical-utilities
number of linear algebra functions such as `inverse`,
`solve`, `transpose`, `chol-decomp`, `lu-decomp` and
`sv-decomp`. These functions are listed in the appendix as well.
-->

An array is printed using the standard Common Lisp format.  For example,
a 2 by 3 matrix with rows (1 2 3) and (4 5 6) is printed as

    #2A((1 2 3)(4 5 6))

The prefix `#2A` indicates that this is a two-dimensional array.  This
form is not particularly readable, but it has the advantage that it can
be pasted into expressions and will be read as an array by the LISP
reader.[^21]  For matrices you can use the function `print-matrix`
to get a slightly more readable representation:

    LS-USER> (print-matrix '#2a((1 2 3)(4 5 6)) *standard-output*)
        1 2 3
        4 5 6
    NIL

The `select` function can be used to extract elements or sub-arrays
from an array. If `A` is a two dimensional array then the
expression

    (select a 0 1)

will return element 1 of row 0 of `A`. The expression

    (select a (list 0 1) (list 0 1))

returns the upper left hand corner of `A`.


<!--
## Nonlinear Regression

LISP-STAT allows the construction of nonlinear, normal regression
models. The present implementation is experimental. The definitions
needed for nonlinear regression are in the file `nonlin.lsp` on the
distribution disk. This file is not loaded automatically at start up;
you should load it now, using the **Load** item on the **File** menu or
the `load` command, to carry out the calculations in this section.

As an example, Bates and Watts [@BatesWatts A1.3] describe an experiment
on the relation between the velocity of an enzymatic reaction, $y$, and
the substrate concentration, $x$. The data for an experiment in which
the enzyme was treated with Puromycin are given by

    (def x1 (list 0.02 0.02 0.06 0.06 .11 .11 .22 .22 .56 .56 1.1 1.1))
    (def y1 (list 76 47 97 107 123 139 159 152 191 201 207 200))

The Michaelis-Menten function
$$\eta(x) = \frac{\theta_{1}x}{\theta_{2}+x}$$ often provides a good
model for the dependence of velocity on substrate concentration.
Assuming the Michaelis-Menten function as the mean velocity at a given
concentration level the function `f1` defined by

    (defun f1 (b) (/ (* (select b 0) x1) (+ (select b 1) x1)))

computes the list of mean response values at the points in `x1` for
a parameter list `b`. Using these definitions, which are contained
in the file `puromycin.lsp` in the `Data` folder of the
distribution disk, we can construct a nonlinear regression model using
the `nreg-model` function.

First we need initial estimates for the two model parameters. Examining
the expression for the Michaelis-Menten model shows that as $x$
increases the function approaches an asymptote, $\theta_{1}$. The second
parameter, $\theta_{2}$, can be interpreted as the value of $x$ at which
the function has reached half its asymptotic value. Using these
interpretations for the parameters and a plot constructed by the
expression

    (plot-points x1 y1)

shown in Figure [\[MikeMentPlot\]](#MikeMentPlot){reference-type="ref"
reference="MikeMentPlot"} we can read off reasonable initial estimates
of 200 for $\theta_{1}$ and 0.1 for $\theta_{2}$. The `nreg-model`
function takes the mean function, the response vector and an initial
estimate of the parameters, computes more accurate estimates using an
iterative algorithm starting from the initial guess, and prints a
summary of the results. It returns a nonlinear regression model object:
[^22]

    > (def puromycin (nreg-model #'f1 y1 (list 200 .1)))
    Residual sum of squares:    7964.19
    Residual sum of squares:    1593.16
    Residual sum of squares:    1201.03
    Residual sum of squares:    1195.51
    Residual sum of squares:    1195.45
    Residual sum of squares:    1195.45
    Residual sum of squares:    1195.45
    Residual sum of squares:    1195.45

    Least Squares Estimates:

    Parameter 0               212.684   (6.94715)
    Parameter 1             0.0641213   (0.00828094)

    R Squared:               0.961261
    Sigma hat:                10.9337
    Number of cases:               12
    Degrees of freedom:            10

    PUROMYCIN
    >

The function `nreg-model` also takes several keyword arguments,
including `:epsilon` to specify a convergence criterion and
`:count-limit`, a limit on the number of iterations. By default
these values are .0001 and 20, respectively. The algorithm for fitting
the model is a simple Gauss-Newton algorithm with backtracking;
derivatives are computed numerically.

To see how you can analyze the model further you can send
`puromycin` the `:help` message. The result is very similar to
the help information for a linear regression model. The reason is
simple: nonlinear regression models have been implemented as objects,
with the nonlinear regression model prototype `nreg-model-proto`
inheriting from the linear regression model prototype. The internal
data, the method for computing estimates, and the method of computing
fitted values have been modified for nonlinear models, but the other
methods remain unchanged. Once the model has been fit the Jacobian of
the mean function at the estimated parameter values is used as the $X$
matrix. In terms of this definition most of the methods for linear
regression, such as the methods `:coef-standard-errors` and
`:leverages`, still make sense, at least as first order asymptotic
approximations.

In addition to the messages for linear regression models a nonlinear
regression model can respond to the messages

    :COUNT-LIMIT
    :EPSILON
    :MEAN-FUNCTION
    :NEW-INITIAL-GUESS
    :PARAMETER-NAMES
    :THETA-HAT
    :VERBOSE

### Exercises {#exercises-7 .unnumbered}

1.  Examine the residuals of the `puromycin` model.

2.  The file `puromycin.lsp` also contains data `x2` and
    `y2` and mean function `f2` for an experiment run without
    the Puromycin treatment. Fit a model to this data and compare the
    results to the experiment with Puromycin.

-->


<!--
## One Way ANOVA

LISP-STAT allows the construction of normal one way analysis of
variance models. The definitions needed are in the file
`oneway.lsp` on the distribution disk. This file is not loaded
automatically at start up; you should load it now, using the **Load**
item on the **File** menu or the `load` command, to carry out the
calculations in this section.

As an example, suppose we would like to model the data on cholesterol
levels in rural and urban Guatemalans, examined in Section
[3.2](#Elementary.Summary){reference-type="ref"
reference="Elementary.Summary"}, as a one way ANOVA model. The box-plots
we obtained in Section [3.2](#Elementary.Summary){reference-type="ref"
reference="Elementary.Summary"} showed that the samples were skewed and
the center and spread of the urban sample were larger than the center
and spread of the rural sample. To compensate for these facts I will use
a normal ANOVA model for the logarithms of the data:

    > (def cholesterol (oneway-model (list (log urban) (log rural))))

    Least Squares Estimates:

    Group 0                 5.377172   (0.03624821)
    Group 1                 5.099592   (0.03456131)

    R Squared:             0.4343646
    Sigma hat:             0.1621069
    Number of cases:              42
    Degrees of freedom:           40

    Group Mean Square:     0.8071994   (1)
    Error MeanSquare:     0.02627865   (40)

    CHOLESTEROL
    >

The function `oneway-model` requires one argument, a list of the
lists or vectors representing the samples for the different groups. The
model `cholesterol` can respond to all regression messages as well
as a few new ones. The new ones are

    :BOXPLOTS
    :ERROR-MEAN-SQUARE
    :ERROR-DF
    :GROUP-DF
    :GROUP-MEAN-SQUARE
    :GROUP-NAMES
    :GROUP-SUM-OF-SQUARES
    :GROUPED-DATA
    :STANDARD-DEVIATIONS

-->

<!--
## Maximization and Maximum Likelihood Estimation {#MaximumLikelihood}

LISP-STAT includes two functions for maximizing functions of several
variables. The definitions needed are in the file `maximize.lisp` on
the distribution disk. This file is not loaded automatically at start
up; you should load it now, using the **Load** item on the **File** menu
or the `load` command, to carry out the calculations in this
section. The material in this section is somewhat more advanced as it
assumes you are familiar with the basic concepts of maximum likelihood
estimation.

Two functions are available for maximization. The first is
`newtonmax`, which takes a function and a list or vector
representing an initial guess for the location of the maximum and
attempts to find the maximum using an algorithm based on Newton's method
with backtracking. The algorithm is based on the unconstrained
minimization system described in Dennis and Schnabel [@DennisSchnabel].

As an example, Cox and Snell [@CoxSnell Example T] describe data
collected on times (in operating hours) between failures of air
conditioning units on several aircraft. The data for one of the aircraft
can be entered as

    (def x (90 10 60 186 61 49 14 24 56 20 79 84 44 59 29 118 25 156 310 76
            26 44 23 62 130 208 70 101 208))

A simple model for these data might be to assume the times between
failures are independent random variables with a common gamma
distribution. The density of the gamma distribution can be written as
$$\frac{(\beta / \mu)(\beta x / \mu)^{\beta - 1} e^{- \beta x / \mu}}
     {\Gamma(\beta)}$$ where $\mu$ is the mean time between failures and
$\beta$ is the gamma shape parameter. The log likelihood for the sample
is thus given by $$n [\log(\beta) - \log(\mu) - \log(\Gamma(\beta))] +
\sum_{i=1}^{n} (\beta - 1) \log(\beta x_{i} / \mu) -
\sum_{i=1}^{n} \beta x_{i} / \mu.$$ We can define a Lisp function to
evaluate this log likelihood by

    (defun gllik (theta)
      (let* ((mu (select theta 0))
             (beta (select theta 1))
             (n (length x))
             (bym (* x (/ beta mu))))
        (+ (* n (- (log beta) (log mu) (log-gamma beta)))
           (sum (* (- beta 1) (log bym)))
           (sum (- bym)))))

This definition uses the function `log-gamma` to evaluate
$\log(\Gamma(\beta))$. The data and function definition are contained in
the file `aircraft.lsp` in the `Data` folder of the
distribution disk.

Closed form maximum likelihood estimates are not available for the shape
parameter of this distribution, but we can use `newtonmax` to
compute estimates numerically. [^23] We need an initial guess to use as
a starting value in the maximization. To get initial estimates we can
compute the mean and standard deviation of `x`

    > (mean x)
    83.5172
    > (standard-deviation x)
    70.8059

and use method of moments estimates $\hat{\mu} = 83.52$ and
$\hat{\beta} = (\hat{\mu} / \hat{\sigma})^{2}$, calculated as

    > (^ (/ (mean x) (standard-deviation x)) 2)
    1.39128

Using these starting values we can now maximize the log likelihood
function:

    > (newtonmax #'gllik (list 83.5 1.4))
    maximizing...
    Iteration 0.
    Criterion value = -155.603
    Iteration 1.
    Criterion value = -155.354
    Iteration 2.
    Criterion value = -155.347
    Iteration 3.
    Criterion value = -155.347
    Reason for termination: gradient size is less than gradient tolerance.
    (83.5173 1.67099)

Some status information is printed as the optimization proceeds. You can
turn this off by supplying the keyword argument `:verbose` with
value `NIL`.

You might want to check that the gradient of the function is indeed
close to zero. If you do not have a closed form expression for the
gradient you can use `numgrad` to calculate a numerical
approximation. For our example,

    > (numgrad #'gllik (list 83.5173 1.67099))
    (-4.07269e-07 -1.25755e-05)

The elements of the gradient are indeed quite close to zero. You can
also compute the second derivative, or Hessian, matrix using
`numhess`. Approximate standard errors of the maximum likelihood
estimates are given by the square roots of the diagonal entries of the
inverse of the negative Hessian matrix at the maximum:

    > (sqrt (diagonal (inverse (- (numhess #'gllik (list 83.5173 1.67099))))))
    (11.9976 0.402648)

Instead of calculating the maximum using `newtonmax` and then
calculating the derivatives separately you can have `newtonmax`
return a list of the location of the maximum, the optimal function
value, the gradient and the Hessian by supplying the keyword argument
`:return-derivs` as `T`. [^24]

Newton's method assumes a function is twice continuously differentiable.
If your function is not smooth or you are having trouble with
`newtonmax` for some other reason you might try a second
maximization function, `nelmeadmax`. `nelmeadmax` takes a
function and an initial guess and attempts to find the maximum using the
Nelder-Mead simplex method as described in Press, Flannery, Teukolsky
and Vetterling [@CRecipes]. The initial guess can consist of a simplex,
a list of $n+1$ points for an $n$-dimensional problem, or it can be a
single point, represented by a list or vector of $n$ numbers. If you
specify a single point you should also use the keyword argument
`:size` to specify as a list or vector of length $n$ the size in
each dimension of the initial simplex. This should represent the size of
an initial range in which the algorithm is to start its search for a
maximum. We can use this method in our gamma example:

    > (nelmeadmax #'gllik (list 83.5 1.4) :size (list 5 .2))
    Value = -155.603
    Value = -155.603
    Value = -155.603
    Value = -155.587
    Value = -155.53
    Value = -155.522
    ...
    Value = -155.347
    Value = -155.347
    Value = -155.347
    Value = -155.347
    (83.5181 1.6709)

It takes somewhat longer than Newton's method but it does reach the same
result.

### Exercises {#exercises-8 .unnumbered}

1.  The data set used in this example consists of sets of measurements
    for ten aircraft. Data for five of the aircraft are contained in the
    variable `failure-times` in the file `aircraft.lsp`. The
    calculations of this section used the data for the second aircraft.
    Examine the data for the remaining four aircraft.

2.  Use maximum likelihood estimation to fit a Weibull model to the data
    used in this section.
-->

<!--
## Approximate Bayesian Computations

This section describes a set of tools for computing approximate
posterior moments and marginal densities in XLISP-STAT. The definitions
needed are in the file `bayes.lsp` on the distribution disk. This
file is not loaded automatically at start up; you should load it now,
using the **Load** item on the **File** menu or the `load` command,
to carry out the calculations in this section. The material in this
section is somewhat more advanced as it assumes you are familiar with
the basic concepts of Bayesian inference.

The functions described in this section can be used to compute first and
second order approximations to posterior moments and saddlepoint-like
approximations to one dimensional marginal posterior densities. The
approximations, based primarily on the results in [@TK; @TKK1; @TKK2],
assume the posterior density is smooth and dominated by a single mode.
The implementation is experimental and may change in a number of ways in
the near future.

Let's start with a simple example, a data set used to study the relation
between survival time (in weeks) of leukemia patients and white blood
cell count recorded for the patients at their entry into the study
[@CoxSnell Example U]. The data consists of two groups of patients
classified as AG positive and AG negative. The data for the 17 AG
positive patients, contained in the file `leukemia.lsp` in the
`Data` folder on the distribution disk, can be entered as

    (def wbc-pos (list 2300 750 4300 2600 6000 10500 10000 17000 5400 7000
                       9400 32000 35000 100000 100000 52000 100000))
    (def times-pos (list 65 156 100 134 16 108 121 4 39 143 56 26 22 1 1 5 65))

A high white blood cell count indicates a more serious stage of the
disease and thus a lower chance of survival.

A model often used for this data assumes the survival times are
exponentially distributed with a mean that is log linear in the
logarithm of the white blood cell count. For convenience I will scale
the white blood cell counts by 10000. That is, the mean survival time
for a patient with white blood cell count $\log(WBC_{i})$ is
$$\mu_{i} = \theta_{0} \exp\{-\theta_{1} x_{i}\},$$ with
$x_{i} = \log(WBC_{i} / 10000)$. The log likelihood function is thus
given by $$\sum_{i=1}^{n} \theta_{1} x_{i}
-n \log(\theta_{0})
-\frac{1}{\theta_{0}} \sum_{i=1}^{n} y_{i} e^{\theta_{1} x_{i}},$$ with
$y_{i}$ representing the survival times. After computing the transformed
$WBC$ variable as

    (def transformed-wbc-pos (- (log wbc-pos) (log 10000)))

the log likelihood can be computed using the function

    (defun llik-pos (theta)
      (let* ((x transformed-wbc-pos)
             (y times-pos)
             (theta0 (select theta 0))
             (theta1 (select theta 1))
             (t1x (* theta1 x)))
        (- (sum t1x)
           (* (length x) (log theta0))
           (/ (sum (* y (exp t1x)))
              theta0))))

I will look at this problem using a vague, improper prior distribution
that is constant over the range $\theta_{i} > 0$; thus the log posterior
density is equal to the log likelihood constructed above, up to an
additive constant. The first step is to construct a Bayes model object
using the function `bayes-model`. This function takes a function
for computing the log posterior density and an initial guess for the
posterior mode, computes the posterior mode by an iterative method
starting with the initial guess, prints a short summary of the
information in the posterior distribution, and returns a model object.
We can use the function `llik-pos` to compute the log posterior
density, so all we need is an initial estimate for the posterior mode.
Since the model we are using assumes a linear relationship between the
logarithm of the mean survival time and the transformed $WBC$ variable a
linear regression of the logarithms of the survival times on
`transformed-wbc-pos` should provide reasonable estimates. The
linear regression gives

    > (regression-model transformed-wbc-pos (log times-pos))

    Least Squares Estimates:

    Constant                  3.54234   (0.302699)
    Variable 0              -0.817801   (0.214047)

    R Squared:                 0.4932
    Sigma hat:                1.23274
    Number of cases:               17
    Degrees of freedom:            15

so reasonable initial estimates of the mode are $\hat{\theta}_{0} =
\exp(3.5)$ and $\hat{\theta}_{1} = 0.8$. Now we can use these estimates
in the `bayes-model` function:

    > (def lk (bayes-model #'llik-pos (list (exp 3.5) .8)))
    maximizing...
    Iteration 0.
    Criterion value = -90.8662
    Iteration 1.
    Criterion value = -85.4065
    Iteration 2.
    Criterion value = -84.0944
    Iteration 3.
    Criterion value = -83.8882
    Iteration 4.
    Criterion value = -83.8774
    Iteration 5.
    Criterion value = -83.8774
    Iteration 6.
    Criterion value = -83.8774
    Reason for termination: gradient size is less than gradient tolerance.


    First Order Approximations to Posterior Moments:

    Parameter 0                56.8489 (13.9713)
    Parameter 1               0.481829 (0.179694)

    #<Object: 1565592, prototype = BAYES-MODEL-PROTO>
    >

It is possible to suppress the summary information by supplying
`NIL` as the value of the `:print` keyword argument.

The summary printed by `bayes-model` gives first order
approximations to the posterior means and standard deviations of the
parameters. That is, the means are approximated by the elements of the
posterior mode and the standard deviations by the square roots of the
diagonal elements of the inverse of the negative Hessian matrix of the
log posterior at the mode. These approximations can also be obtained
from the model by sending it the `:1stmoments` message:

    > (send lk :1stmoments)
    ((56.8489 0.481829) (13.9713 0.179694))

The result is a list of two lists, the means and the standard
deviations. A slower but more accurate second order approximation is
available as well. It can be obtained using the message `:moments`:

    > (send lk :moments)
    ((65.3085 0.485295) (17.158 0.186587))

Both these messages allow you to compute moments for individual
parameters or groups of parameters by specifying an individual parameter
index or a list of indices:

    > (send lk :moments 0)
    ((65.3085) (17.158))
    > (send lk :moments (list 0 1))
    ((65.3085 0.485295) (17.158 0.186587))

The first and second order approximations to the moments of $\theta_{0}$
are somewhat different; in particular the mean appears to be somewhat
larger than the mode. This suggests that the posterior distribution of
this parameter is skewed to the right. We can confirm this by looking at
a plot of the approximate marginal posterior density. The message
`:margin1` takes a parameter index, and a sequence of points at
which to evaluate the density and returns as its value a list of the
supplied sequence and the approximate density values at these points.
This list can be given to `plot-lines` to produce a plot of the
marginal density:

    > (plot-lines (send lk :margin1 0 (rseq 30 120 30)))
    #<Object: 1623804, prototype = SCATTERPLOT-PROTO>

The result is shown in Figure [\[PMar0\]](#PMar0){reference-type="ref"
reference="PMar0"} and does indeed show some skewness to the right.

In addition to examining individual parameters it is also possible to
look at the posterior distribution of smooth functions of the
parameters.[^25] For example, you might want to ask what information the
data contains about the probability of a patient with $WBC = 50000$
surviving a year or more. This probability is given by
$$\frac{1}{\mu(x)}e^{-52 / \mu(x)},$$ with
$$\mu(x) = \theta_{0} e^{-\theta_{1} x}$$ and $x = \log(5)$, and can be
computed by the function

    (defun lk-sprob (theta)
      (let* ((time 52.0)
             (x (log 5))
             (mu (* (select theta 0) (exp (- (* (select theta 1) x))))))
        (exp (- (/ time mu)))))

This function can be given to the `:1stmoments`, `:moments`
and `:margin1` methods to approximate the posterior moments and
marginal posterior density of this function. For the moments the results
are

    > (send lk :1stmoments #'lk-sprob)
    ((0.137189) (0.0948248))
    > (send lk :moments #'lk-sprob)
    ((0.184275) (0.111182))

with the difference again suggesting some positive skewness, and the
marginal density produced by the expression

    (plot-lines (send lk :margin1 #'lk-sprob (rseq .01 .8 30)))

is shown in Figure [\[PMarP\]](#PMarP){reference-type="ref"
reference="PMarP"}. Based on this picture the data suggests that this
survival probability is almost certainly below 0.5, but it is difficult
to make a more precise statement than that.

The functions described in this section are based on the optimization
code described in the previous section. By default derivatives are
computed numerically. If you can compute derivatives yourself you can
have your log posterior function return a list of the function value and
the gradient or a list of the function value, the gradient and the
Hessian matrix.

### Exercises {#exercises-9 .unnumbered}

1.  To be able to think about prior distributions for the two parameters
    in this problem we need to try to understand what the parameters
    represent. The parameter $\theta_{0}$ is fairly easy to understand:
    it is the mean survival time for patients with $WBC = 10000$. The
    parameter $\theta_{0}$ is a little more difficult to think about. In
    represents the approximate percent difference in mean survival time
    for patients with $WBC$ differing by one percent. Because of the
    minus sign in the mean relationship, and the expected inverse
    relation between $WBC$ and mean survival time, $\theta_{1}$ is
    expected to be positive.

    Consider an informative prior distribution that assumes the two
    parameters *a priori* independent, takes $\log(\theta_{0})$ to be
    normally distributed with mean $\log(52)$ and standard deviation
    $\log(2)$, and $\theta_{1}$ to be exponentially distributed with
    mean $\mu = 5$. This prior is designed to represent an opinion that
    mean survival time at $WBC = 10000$ should be around one year, but
    that guess could easily be off by a factor of two either way. The
    percentage change in the mean for a one percent change in $WBC$
    should be on the order of one to ten or so. Examine the posterior
    distribution for this prior and compare your results to the results
    for the vague prior used above.

2.  Construct and examine a posterior distribution for the parameters of
    the gamma model based on the aircraft data of Section
    [12](#MaximumLikelihood){reference-type="ref"
    reference="MaximumLikelihood"}.
-->

## References

Bates, D. M. and Watts, D. G., (1988), *Nonlinear Regression Analysis
and its Applications*, New York: Wiley.

Becker, Richard A., and Chambers, John M., (1984), *S: An Interactive
Environment for Data Analysis and Graphics*, Belmont, Ca: Wadsworth.

Becker, Richard A., Chambers, John M., and Wilks, Allan R., (1988), *The
New S Language: A Programming Environment for Data Analysis and
Graphics*, Pacific Grove, Ca: Wadsworth.

Becker, Richard A., and William S. Cleveland, (1987), "Brushing
scatterplots," *Technometrics*, vol. 29, pp. 127-142.

Betz, David, (1985) "An XLISP Tutorial," *BYTE*, pp 221.

Betz, David, (1988), "XLISP: An experimental object-oriented programming
language," Reference manual for XLISP Version 2.0.

Chaloner, Kathryn, and Brant, Rollin, (1988) "A Bayesian approach to
outlier detection and residual analysis," *Biometrika*, vol. 75, pp.
651-660.

Cleveland, W. S. and McGill, M. E., (1988) *Dynamic Graphics for
Statistics*, Belmont, Ca.: Wadsworth.

Cox, D. R. and Snell, E. J., (1981) *Applied Statistics: Principles and
Examples*, London: Chapman and Hall.

Dennis, J. E. and Schnabel, R. B., (1983), *Numerical Methods for
Unconstrained Optimization and Nonlinear Equations*, Englewood Cliffs,
N.J.: Prentice-Hall.

Devore, J. and Peck, R., (1986), *Statistics, the Exploration and
Analysis of Data*, St. Paul, Mn: West Publishing Co.

McDonald, J. A., (1982), "Interactive Graphics for Data Analysis,"
unpublished Ph. D. thesis, Department of Statistics, Stanford
University.

Oehlert, Gary W., (1987), "MacAnova User's Guide," Technical Report 493,
School of Statistics, University of Minnesota.

Press, Flannery, Teukolsky and Vetterling, (1988), *Numerical Recipes in
C*, Cambridge: Cambridge University Press.

Steele, Guy L., (1984), *Common Lisp: The Language*, Bedford, MA:
Digital Press.

Stuetzle, W., (1987), "Plot windows," *J. Amer. Statist. Assoc.*, vol.
82, pp. 466 - 475.

Tierney, Luke, (1990) *LISP-STAT: Statistical Computing and Dynamic
Graphics in Lisp*. Forthcoming.

Tierney, L. and J. B. Kadane, (1986), "Accurate approximations for
posterior moments and marginal densities," *J. Amer. Statist. Assoc.*,
vol. 81, pp. 82-86.

Tierney, Luke, Robert E. Kass, and Joseph B. Kadane, (1989), "Fully
exponential Laplace approximations to expectations and variances of
nonpositive functions," *J. Amer. Statist. Assoc.*, to appear.

Tierney, L., Kass, R. E., and Kadane, J. B., (1989), "Approximate
marginal densities for nonlinear functions," *Biometrika*, to appear.

Weisberg, Sanford, (1982), "MULTREG Users Manual," Technical Report 298,
School of Statistics, University of Minnesota.

Winston, Patrick H. and Berthold K. P. Horn, (1988), *LISP*, 3rd Ed.,
New York: Addison-Wesley.


## Appendix A: LISP-STAT Interface to the Operating System
<!--
This tutorial has dealt primarily with the Macintosh version of
XLISP-STAT. XLISP-STAT is also available on UNIX systems. If it has been
installed in a directory in your search path you should be able to start
it up by typing

    xlispstat

at the UNIX shell level. There are a few differences between the
Macintosh and UNIX versions. On UNIX systems:

-   UNIX versions of XLISP-STAT are designed to run on a standard
    terminal and therefore do not provide parenthesis matching or
    indentation support. If you use the *GNU* `Emacs` editor you
    can obtain both these features by running XLISP-STAT from within
    `Emacs`. Otherwise, for editing files with `vi` you can
    use the `-l` flag to get some Lisp editing support.

-   To quit from the program type

        (exit)

    On most systems you can also quit by typing a *Control-D*.

-   You can interrupt a calculation that is taking too long or was
    started in error by typing a *Control-C*.

-   Data and example files are stored in the `Data` and
    `Examples` subdirectories of the library tree. The functions
    `load-data` and `load-examples` will look in these
    directories, so

        (load-data "tutorial")

    will load the data sets for the tutorial. Within XLISP-STAT the
    variable `*default-path*` shows the root directory of the
    library; you can look there if you want to examine the example
    files.

-   The `require` function can be used to load program modules not
    loaded at startup. To load the nonlinear regression module, for
    example, use the expression

        (require "nonlin")

On basic UNIX systems the only graphics available are the functions
`plot-points` and `plot-lines`. These functions assume you are
using a *Tektronix* terminal or emulator.
-->

### A.1 Running System Commands from LISP-STAT

The `run-program` function can be used to run UNIX commands from within
LISP-STAT.  This function takes a shell command string as its argument
and returns the shell exit code for the command.  For example, you can
print the date using the UNIX `date` command:

    LS-USER> (uiop:run-program "date" :output *standard-output*)
    Wed Jul 19 11:06:53 CDT 1989
    0


The return value is 0, indicating successful completion of the UNIX
command.

<!--
### A.2 Dynamic Loading and Foreign Function Calling

Most Common Lisp implementations provide a facility to allow
you to use your own C functions or FORTRAN subroutines from within
LISP-STAT. The facility, patterned after the approach used in the New
*S* language [@newSbook], consists of the function `dyn-load` for
loading your code into a running LISP-STAT process and the functions
`call-cfun`, `call-fsub` and `call-lfun` for calling
subroutines in the loaded code. The `dyn-load` function requires
one argument, a string containing the name of a file to be linked and
loaded. The file will be linked with standard C libraries before
loading. If you need it to be linked with the standard FORTRAN libraries
as well you can give the keyword argument `:fortran` the value
`T`. Finally, if you need to link other libraries you can supply a
string containing the library flags you would specify in a linking
command as the value of the keyword argument `:libflags`. For
example, to include the library `cmlib` use the string
`"-lcmlib"`.[^26]

The function `call-cfun` takes a string identifying the C function
you want to call, followed by additional arguments for your C function.
The additional arguments must be integers, sequences of integers, real
numbers or sequences of real numbers. Pointers to `int` or
`double` data containing these values will be passed to your
routine. After your routine returns the contents of the data referred to
by these pointers are copied into lists and `call-cfun` returns a
list of these lists.

As an example, suppose the file `foo.c` contains the following C
function:

    foo(n, x, sum)
         int *n;
         double *x, *sum;
    {
      int i;

      for (i = 0, *sum = 0.0; i < *n; i++) {
        *sum += x[i];
      }
    }

After compiling the file to `foo.o` we can load it into XLISP-STAT
using the expression

    (dyn-load "foo.o")

We can then call the function `foo` using a list of real numbers as
the second argument. The function `float` can be used to coerce
numbers to reals:

    > (call-cfun "foo" 5 (float (iseq 1 5)) 0.0)
    ((5) (1 2 3 4 5) (15))

The third argument to `foo` has been used to return the result.

The function `call-fsub` is used for calling FORTRAN subroutines
that have been loaded dynamically. A FORTRAN subroutine analogous to the
C function `foo` might be written as

          subroutine foo(n, x, sum)
          integer n
          double precision x(n), sum

          integer i

          sum = 0.0
          do 10 i = 1, n
             sum = sum + x(i)
     10   continue
          return
          end

After compiling and loading this routine it can be called using
`call-fsub`:

    > (call-fsub "foo" 5 (float (iseq 1 5)) 0.0)
    ((5) (1 2 3 4 5) (15))

Two C functions you may want to call from within your C functions are
`xscall_alloc` and `xscall_fail`. The function
`xscall_alloc` is like `calloc`, except it insures the
allocated memory is garbage collected after the call to `call-cfun`
returns. The function `xscall_fail` takes a character string as its
argument. It prints the string and signals an error.

The function `call-lfun` can be used to call C functions written
using the internal XLISP conventions for obtaining arguments and
returning results. This allows you to accept any kinds of arguments.
Unfortunately, the source code is the only documentation for the
internal calling conventions.

A note of caution may be appropriate at this point. Dynamically loaded
code contains only the error checking you build into it. If a function
is not called with the proper arguments it will most likely cause
XLISP-STAT to crash, losing any variables you have not saved.

At present the number of arguments you can pass to C functions or
FORTRAN subroutines using `call-cfun` or `call-fsubr` is
limited to 15.

If dynloading is not available on your system you can still recompile
XLISP-STAT with files of your own added to the source code. The
functions `call-cfun`, `call-fsubr` and `call-lfun` can
be used to call your functions or subroutines in this case as well.
-->

<!--
## Graphical Interface Tools

One of the characteristic features of the Macintosh user interface is
the use of menus and dialogs for interacting with the computer.
XLISP-STAT allows you to construct your own menus and dialogs using Lisp
commands. This appendix gives a very brief introduction to constructing
menus and dialogs; further details will be given in [@MyBook]. A few of
the explanations and examples in this appendix use Lisp concepts that
have not been covered in this tutorial.

### Menus

As an illustration I will outline how to construct a menu for sending
some simple messages to a regression model. I will make the convention
that there is a *current regression model*, the value of the symbol
`*current-model*`.

Menus are created by sending the `:new` message to the menu
prototype, `menu-proto`. The method for this message takes a single
argument, the menu title. We can use this message to set up our menu:

    > (setf model-menu (send menu-proto :new "Model"))
    #<Object: 4055334, prototype = MENU-PROTO>

Macintosh menus can be installed in and removed from the menu bar by
sending them the `:install` and `:remove` messages:

    > (send model-menu :install)
    NIL
    > (send model-menu :remove)
    NIL

On other systems menus are *popped up*; this can be accomplished by
sending the `:popup` message to a menu. This message requires two
arguments, the $x$ and $y$ pixel coordinates of the left top corner of
the menu, measured from the left top corner of the screen.

Initially the menu has no items in it. Items are created using the
`menu-item-proto` prototype. The initialization method requires one
argument, the item's title, and takes several keyword arguments,
including

-   `:action` -- a function to be called when the item is selected

-   `:enabled` -- true by default

-   `:key` -- a character to serve as the keyboard equivalent

-   `:mark` -- `nil` (the default) or `t` to indicate a
    check mark.

Analogous messages are available for changing these values in existing
menu items.

Suppose we would like to be able to use our menu to print a summary of
the current model or obtain a residual plot. We can construct two menu
items:

    > (setf summary (send menu-item-proto :new "Summary" :action
                       #'(lambda () (send *current-model* :display))))
    #<Object: 4034406, prototype = MENU-ITEM-PROTO>
    > (setf plot (send menu-item-proto :new "Plot Residuals" :action 
                       #'(lambda () (send *current-model* :plot-residuals))))
    #<Object: 3868686, prototype = MENU-ITEM-PROTO>

Suppose we have assigned the `bikes2` model of Section
[7](#Regression){reference-type="ref" reference="Regression"} to
`*current-model*`. You can force an item's action to be invoked by
sending it the `:do-action` message from the listener:

    > (send summary :do-action)

    Least Squares Estimates:

    Constant               -16.41924   (7.848271)
    Variable 0              2.432667   (0.9719628)
    Variable 1           -0.05339121   (0.02922567)

    R Squared:             0.9477923
    Sigma hat:             0.5120859
    Number of cases:              10
    Degrees of freedom:            7

    NIL

Ordinarily you will not send this message this way: the system sends
this message to the menu item when you select the item from a menu.

To add these items to the menu use the `:append-items` message:

    > (send model-menu :append-items summary plot)
    NIL

You can also use the `:append-items` message to add items to a plot
menu. The menu associated with a plot can be obtained by sending the
plot the `:menu` message with no arguments.

You can enable and disable a menu item with the `:enabled` message:

    > (send summary :enabled)
    T
    > (send summary :enabled nil)
    NIL
    > (send summary :enabled t)
    T

### Dialogs

Dialogs are similar to menus in that they are based on a dialog
prototype and dialog item prototypes. There are, however many more
variations. Fortunately most dialogs you need fall into one of several
categories and can be produced by custom dialog construction functions.

#### Modal Dialogs

Modal dialogs are designed to ask specific questions and wait until they
receive a response. All other interaction is disabled until the dialog
is dismissed -- they place the system in *dialog mode*. Six functions
are available for producing some standard modal dialogs:

-   `(message-dialog <string>)` -- presents a message with an
    **OK** button; returns `nil` when the button is pressed.

-   `(ok-or-cancel-dialog <string>)` -- presents a message with an
    **OK** and a **Cancel** button; returns `t` or `NIL`
    according to the button pressed.

-   `(choose-item-dialog <string> <string-list>)` -- presents a
    heading and a set of radio buttons for choosing one of the strings.
    Returns the index of the selected string on **OK** or `nil` on
    **Cancel**. Example:

        > (choose-item-dialog "Dependent variable:" '("X" "Y" "Z"))
        1

-   `(choose-subset-dialog <string> <string-list>)` -- presents a
    heading and a set of check boxes for indicating which items to
    select. Returns a list of the list of selected indices on **OK** or
    `nil` on **Cancel**. Example:

        > (choose-subset-dialog "Independent variables:" '("X" "Y" "Z"))
        ((0 2))

-   `(get-string-dialog <prompt> [:initial <expr>])` -- presents a
    dialog with a prompt, an editable text field, an **OK** and a
    **Cancel** button. The initial contents of the editable field is
    empty or the `princ` formatted version of $<$*expr*$>$. The
    result is a string or `nil`. Example:

        > (get-string-dialog "New variable label:" :initial "X")
        "Tensile Strength"

-   `(get-value-dialog <prompt> [:initial <expr>])` -- like
    `get-string-dialog`, except

    -   the initial value expression is converted to a string with
        `print` formatting

    -   the result is interpreted as a lisp expression and is evaluated

    -   the result is a list of the value, or `nil`

On the Macintosh there are two additional dialogs for dealing with
files:

-   `(open-file-dialog)` -- presents a standard **Open File**
    dialog and returns a file name string or `nil`. Resets the
    working folder on **OK**.

-   `(set-file-dialog prompt)` -- presents a standard **Save
    File** dialog. Returns a file name string or `nil`. Resets the
    working folder on **OK**.

#### Modeless Dialogs

Two functions for constructing custom modeless dialogs are available
also. They are the functions `interval-slider-dialog` and
`sequence-slider-dialog` introduced above in Section
[8](#Fundefs){reference-type="ref" reference="Fundefs"}.
-->

[^1]: It is possible to make a finer distinction. The *reader* takes a
    string of characters from the listener and converts it into an
    expression. The *evaluator* evaluates the expression and the
    *printer* converts the result into another string of characters for
    the listener to print. For simplicity I will use *evaluator* to
    describe the combination of these functions.

[^2]: `def` acts like a special form, rather than a function, since
    its first argument is not evaluated (otherwise you would have to
    quote the symbol). Technically `def` is a macro, not a special
    form, but I will not worry about the distinction in this tutorial.
    `def` is closely related to the standard Lisp special forms
    `setf` and `setq`. The advantage of using `def` is
    that it adds your variable name to a list of `def`'ed variables
    that you can retrieve using the function `variables`. If you
    use `setf` or `setq` there is no easy way to find
    variables you have defined, as opposed to ones that are predefined.
    `def` always affects top level symbol bindings, not local
    bindings. It cannot be used in function definitions to change local
    bindings.

[^3]: Use the function `load`. For example, evaluating the expression
    `(load #P"LS:DATASETS;CAR-PRICES")` should load the file
    `car-prices.lisp`.

[^4]: As an aside, a Lisp symbol can be thought of as a "thing" with
    four cells. These cells contain the symbol's print name, its value,
    its function definition, and its property list. Lisp symbols are
    thus much more like physical entities than variable identifiers in
    FORTRAN or C.

[^5]: The generator used is Marsaglia's portable generator from the
    *Core Math Libraries* distributed by the National Bureau of
    Standards. A state object is a vector containing the state
    information of the generator. "Random" reseeding occurs off the
    system clock.

[^6]: Help is available both in the REPL, and online at
    https://lisp-stat.dev/

[^7]: This process of applying a function elementwise to its arguments
    is called *mapping*.

[^8]: The notation used corresponds to the specification of the
    argument lists in Lisp function definitions. See Section
    [8](#Fundefs){reference-type="ref" reference="Fundefs"} for more
    information on defining functions.

[^9]: Note that the keyword `:title` has not been quoted. *Keyword
    symbols*, symbols starting with a colon, are somewhat special. When
    a keyword symbol is created its value is set to itself. Thus a
    keyword symbol effectively evaluates to itself and does not need to
    be quoted.

[^10]: Actually `pi` represents a constant, produced with
    `defconst`. Its value cannot be changed by simple assignment.

[^11]: I have used a quoted list `’(purchases precipitation)` in
    this expression to pass the list of symbols to the `savevar`
    function. A longer alternative would be the expression
    `(list ’purchases ’precipitation).`

[^12]: According to Stuetzle [@Stuetzle] the idea to link several plots
    was first suggested by McDonald [@McDonaldThesis].

[^13]: The result returned by `plot-points` is printed something
    like `#<Object: 2010278, prototype = SCATTERPLOT-PROTO>`. This
    is not the value returned by the function, just its printed
    representation . There are several other data types that are printed
    this way; *file streams*, as returned by the `open` function,
    are one example. For the most part you can ignore these printed
    results. There is one unfortunate feature, however: the form
    `#<...>` means that there is no printed form of this data type
    that the Lisp reader can understand. As a result, if you forget to
    give your plot a name you can't cut and paste the result into a
    `def` expression -- you have to redo the plot or use the
    history mechanism.

[^14]: To keep things simple I will use the term *message* to refer to a
    message corresponding to a message selector.

[^15]: `dotimes` is one of several Lisp looping constructs. It is a
    special form with the syntax `(dotimes (var count) expr ....)`.
    The loop is repeated `count` times, with `var` bound to 0,
    1, ..., `count` - 1. Other looping constructs are
    `dolist`, `do` and `do*`.

[^16]: Recall from Section
    [6.5](#MorePlots.Modifying){reference-type="ref"
    reference="MorePlots.Modifying"} that
    `#<Object: 1966006, prototype = REGRESSION-MODEL-PROTO>` is the
    printed representation of the model object returned by
    `regression-model`. Unfortunately you can't cut and paste it
    into the `def`, but of course you can cut and paste the
    `regression-model` expression or use the history mechanism.

[^17]: Ordinarily the entries in the lists returned by these messages
    correspond simply to the intercept, if one is included in the model,
    followed by the independent variables as they were supplied to
    `regression-model`. However, if degeneracy is detected during
    computations some variables will not be used in the fit; they will
    be marked as *aliased* in the printed summary. The indices of the
    variables used can be obtained by the `:basis` message; the
    entries in the list returned by `:coef-estimates` correspond to
    the intercept, if appropriate, followed by the coefficients of the
    elements in the basis. The messages `:x-matrix` and
    `:xtxinv` are similar in that they use only the variables in
    the basis.

[^18]: The / function is used here with three arguments. The first
    argument is divided by the second, and the result is then divided by
    the third. Thus the result of the expression (/ 6 3 2) is 1.

[^19]: The discussion in this section only scratches the surface of what
    you can do with functions in the XLISP language. To see more
    examples you can look at the files that are loaded when XLISP-STAT
    starts up. For more information on options of function definition,
    macros, etc. see the XLISP documentation and the books on Lisp
    mentioned in the references.

[^20]: `mapcar` can be given several lists after the function. The
    function must take as many arguments as there are lists.
    `mapcar` will apply the function using the first element of
    each list, then using the second element, and so on, until one of
    the lists is exhausted, and return a list of the results.

[^21]: You should quote an array if you type it in using this form, as
    the value of an array is not defined.

[^22]: Recall that the expression `#’f1` is short for
    `(function f1)` and is used for obtaining the function
    definition associated with the symbol `f1`.

[^23]: The maximizing value for $\mu$ is always the sample mean. We
    could take advantage of this fact and reduce the problem to a one
    dimensional maximization problem, but it is simpler to just maximize
    over both parameters.

[^24]: The function `newtonmax` ordinarily uses numerical
    derivatives in its computations. Occasionally this may not be
    accurate enough or may take too long. If you have an expression for
    computing the gradient or the Hessian then you can use these by
    having your function return a list of the function value and the
    gradient, or a list of the function value, the gradient and the
    Hessian matrix, instead of just returning the function value.

[^25]: The approximation methods assume these functions are twice
    continuously differentiable; thus they can not be indicator
    functions.

[^26]: There may be slight differences in the implementation of
    `dyn-load` on different systems. The help information for this
    function should give information that is appropriate for your
    system.
