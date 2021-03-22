---
title: "What is Lisp-Stat?"
author: ["Steven Nunez"]
description: "A statistical analysis environment written in Common Lisp"
draft: false
linkTitle: "Overview"
weight: 1
---

Lisp-Stat is a domain specific language (DSL) for statistical analysis
and machine learning. It is targeted at statistics practioners with
little or no experience in programming.

Lisp has a history of being deployed for domain experts to use, and
it's a great language for beginners; the [Symbolics Graphics
Division](https://en.wikipedia.org/wiki/Symbolics#Symbolics_Graphics_Division)
wrote the software used by graphic artists to develop scenes in
several films prior the rise of Pixar. One of the first statistical
systems developed,
[XLisp-Stat](https://en.wikipedia.org/wiki/XLispStat), was a
contemporary until the primary author joined the 'R Core' group.


## Raisons d'être {#raisons-d-être}

There are several reasons to prefer Lisp-Stat over R or Python.  The
first is that it is fast. Lisp compilers produce native executable
code that is nearly as fast as C.  As part of the Common Lisp
[numerical tower](https://en.wikipedia.org/wiki/Numerical_tower)
it also has built in rational numbers, which is a natural way to work
with samples.  For example an experiment may produce 11598 positives
out of a sample of 25000.  With exact rational arithmatic, there is no
need to force everything to a float, the value is just what the
experiment said: 11598 / 25000.

Probably the most important reason though is given in the paper by
Ross Ihaka, one of the originators of the R language, [Lisp as a Base
for a Statistical Computing
System](https://www.stat.auckland.ac.nz/~ihaka/downloads/Compstat-2008.pdf)
about the deficiencies in R and the inability to compile to
machine code (among other issues). The same is true of Python. In that
paper he argues for Lisp as a replacement for R.

Not only does Common Lisp provide a compiler that produces machine
code, it has native threading, a rich ecosystem of code libraries, and
a history of industrial deployments, including:

- Credit card authorisation at Amex (Authorizers Assistant)
- US DoD logistics (and more that we do not know of)
- CIA and NSA are big users based on Lisp sales
- DWave and Rigetti use lisp for programming their quantum computers
- Apple's Siri was originally written in Lisp
- Amazon got started with Lisp & C; so did Y-combinator
- Google's flight search engine is written in Common Lisp
- AT&T used a stripped down version of Symbolics Lisp to process CDRs in the first IP telephony switches

If Lisp is good enough for those applications, it very likely can meet
the needs of an enterprise deployment today.

## Library Consolidation

Eventually, we aim for a consolidation of lisp statistical libraries
in the hopes of achieving a critical mass in the domain.  The reasons
for moving in this direction were described in an article some years
ago entitled [Consolidating Common Lisp
Libraries](https://fare.livejournal.com/169346.html).  Whilst
historical precedent is against us in this movement, that does not
mean we won't try.  If any readers would like to merge the Lisp-Stat
library into a larger, consolidated work, please make contact; we will
be happy to help.


## Core Systems {#systems}
Lisp-Stat is composed of several systems (projects), each
independently useful and brought together under the Lisp-Stat
system. Dependencies between systems have been minimised to the extent
possible possible so you can use them without importing all of
Lisp-Stat.

### Data-Frame {#data-frame}

A data frame is a data structure conceptually similar to a [R data
frame](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame).
It provides column-centric storage for data sets where each named
column contains the values for one variable, and each row contains one
set of observations.

Data frame can contain values of any type. If desired, additional
attributes, such as float, the unit and other information may be
attached to the variable for convenience or efficiency. For example
you could specify a unit of length, say m/s (meters per second), to
ensure that mathmatical operations on that variable always produce
lengths (though the unit may change).

### DFIO {#dfio}

The Data Frame I/O system provides input and output operations for
data frames. Based on the
[cl-csv](https://github.com/AccelerationNet/cl-csv) library, a data
frame may be written to and read from files, strings or streams,
including network streams.  Various separators and escapes are
availble in the `cl-csv` package.


### Select {#select}

Select is a facility for selecting portions of sequences or arrays. It provides:

- An API for making selections (elements selected by the Cartesian
  product of vectors of subscripts for each axis) of array-like
  objects. The most important function is `select`.  Unless you want
  to define additional methods for `select`, this is pretty much all
  you need from this library.
- An extensible DSL for selecting a subset of valid subscripts.  This
  is useful if, for example, you want to resolve column names in a
  data frame in your implementation of select.

### Array Operations {#aops}

This library is a collection of functions and macros for manipulating
Common Lisp arrays and performing numerical calculations with
them. The library provides shorthand codes for frequently used
operations, displaced array functions, indexing, transformations
generation, permutation and reduction of columns.  Array operations
may also be applied to data frames, and data frames may be converted
to/from arrays.

### Special Functions {#special-functions}

This library implements numerical [special
functions](https://en.wikipedia.org/wiki/Special_functions) in Common
Lisp with a focus on high accuracy double-float calculations.  These
functions are the basis for the statistical distributions functions,
e.g. gamma, beta, etc.


### Distributions {#distributions}

A library for probability distributions and associated functions, with
an emphasis on accuracy and correctness.  It provides a consistent
interface to the distributions and computes:

- Probablility density/mass functions (PDF) and log-pdf
- Cumulative density functions (CDF)
- quantiles
- random draws from distributions
- mean, variance of distributions


### Numerical Utilities {#numerical-utilities}

[Numerical Utilities](https://github.com/Lisp-Stat/numerical-utilities) is the
base system that most others depend on. It is a collection of packages
providing:

  - `num=`, et. al. comparison operators for floats
  - simple arithmetic functions, like `sum` and `l2norm`
  - element-wise operations for arrays and vectors
  - intervals
  - special matrices and shorthand for their input
  - sample statistics
  - Chebyshev polynomials
  - quadratures
  - univariate root finding
  - horner's, simpson's and other functions for numerical analysis

### Lisp-Stat {#lisp-stat}

This is the top level package that uses the other packages to create a
statistical computing environment.  It is also the location for the
'unified' interface, where the holes are plugged with third party
packages. For example
[cl-mathstats](https://github.com/gwkkwg/cl-mathstats) contains
functionality not yet in Lisp-Stat, however its architecture does not
lend itself well to incorporation via a `depends-on`, so as we
consolidate the libraries, missing functionality will be placed in the
Lisp-Stat system.  Eventually parts of `numerical-utilities`,
especially the statistics functions, will be relocated here.


## IDEs {#ides}


### Emacs {#emacs}

Emacs, with the [slime](https://common-lisp.net/project/slime/)
package is the most tested IDE and the one the authors use.  If you
are using one of the starter lisp packages mentioned in the [getting
started](/docs/getting-started) section, this will have been installed
for you. Otherwise, slime/swank is available in quicklisp.

### Jupyter Lab {#jupyter-lab}

[Jupyter Lab](http://jupyterlab.io/) and
[common-lisp-jupyter](https://github.com/yitzchak/common-lisp-jupyter)
provide an environment similar to RStudio for working with data and
performing analysis.  The [Lisp-Stat examples](/docs/examples) use
Jupyter Lab to illustrate worked examples based on the book,
*Introduction to the Practice of Statistics*.

### Clozure Common Lisp {#ccl}

On MacOS, [Clozure Common Lisp](https://github.com/Clozure/ccl),
provides a graphical editing environment with a built-in editor and
menu driven system for working with Lisp-Stat.

## Roadmap

Generally, we are prioritising these systems for development:

1. Data Frame
2. Plotting
3. Special Functions & Distributions

In terms of priority, 1 & 2 are equally rated, and
special-functions/distributions lower priority because we have a few
options for them, such as the CFFI for libRmath or lesser quality lisp
implementations.  As well, the knowledge of numerical methods required
for accurate implementation is somewhat more limited.

For the most part, implementation priority is determined by the
features required when working through the [Lisp-Stat
examples](/docs/examples/) and the [basic
tutorial](/docs/tutorials/basics).  Being able to execute the code in
both of these documents is the first MVP milestone. If you see
something in one of these documents that does not work yet it will be
a good starter issue for a contribution.

For data frames, we use the
'[tibble](https://www.rdocumentation.org/packages/tibble/versions/3.1.0)'
from R as inspiration for functionality. Skeletal [documentation for
data frame](/docs/tasks/data-frame/) serves as a roadmap for remaining
features.  If there's something in the table of contents that is not
documented, it means it is not yet implemented.  Please open an issue
on github if you'd like to contribute to any of these features, as
several have partial implementations already.


## Acknowledgements {#acknowledgements}

Tamas Papp was the original author of many of these
libraries.  Starting with relatively clean, working, code that solves
real-world problems was a great start to the development of Lisp-Stat.


## What next?

- [Get started](/docs/getting-started/): Install and run Lisp-Stat
- [Examples](/docs/examples): Worked examples of statistical analysis with Lisp-Stat
