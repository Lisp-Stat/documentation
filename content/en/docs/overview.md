---
title: "What is Lisp-Stat?"
author: ["Steven Nunez"]
description: "A statistical computing environment written in Common Lisp"
draft: false
linkTitle: "Overview"
weight: 1
---

Lisp-Stat is a domain specific language (DSL) for statistical analysis
and machine learning.  It is targeted at statistics practitioners with
little or no experience in programming.

<!--
Lisp has a history of being deployed for domain experts to use, and
it's a great language for beginners; the [Symbolics Graphics
Division](https://en.wikipedia.org/wiki/Symbolics#Symbolics_Graphics_Division)
wrote the software used by graphic artists to develop scenes in
several films prior the rise of Pixar. One of the first statistical
systems developed,
[XLisp-Stat](https://en.wikipedia.org/wiki/XLispStat), was a
contemporary to R until the primary author joined the 'R Core' group.


## Raisons d'être {#raisons-d-être}

There are several reasons to prefer Lisp-Stat over R or Python.  The
first is that it is fast. Lisp compilers produce native executable
code that is nearly as fast as C.  The Common Lisp
[numerical tower](https://en.wikipedia.org/wiki/Numerical_tower)
has support for rational numbers, which is a natural way to work
with samples.  For example an experiment may produce 11598 positives
out of a sample of 25000.  With exact rational arithmetic, there is no
need to force everything to a float, the value is just what the
experiment said: 11598 / 25000.

Not only does Common Lisp provide a compiler that produces machine
code, it has native threading, a rich ecosystem of code libraries, and
a history of industrial deployments, including:

- Credit card authorisation at Amex (Authorizers Assistant)
- US DoD logistics (and more that we do not know of)
- CIA and NSA are big users based on Lisp sales
- DWave, HSL and Rigetti use lisp for programming their quantum computers
- Apple's Siri was originally written in Lisp
- Amazon got started with Lisp & C; so did Y-combinator
- Google's flight search engine is written in Common Lisp
- AT&T used a stripped down version of Symbolics Lisp to process CDRs in the first IP telephony switches

If Lisp is good enough for those applications, it very likely can meet
the needs of an enterprise deployment today.
-->

## Relationship to XLISP-Stat

Although inspired by Tierney's XLisp-Stat, this is a reboot in Common
Lisp.  XLisp-Stat code is unlikely to run except in trivial cases.
Existing XLisp-Stat libraries can be ported with the assistance of the
[XLS-Compat](https://github.com/Lisp-Stat/XLS-compat) system.


## Core Systems {#systems}
Lisp-Stat is composed of several systems (projects), each
independently useful and brought together under the Lisp-Stat
umbrella. Dependencies between systems have been minimised to the
extent possible so you can use them individually without importing all
of Lisp-Stat.

### Data-Frame {#data-frame}

A data frame is a data structure conceptually similar to a [R data
frame](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame).
It provides column-centric storage for data sets where each named
column contains the values for one variable, and each row contains one
set of observations. For data frames, we use the
'[tibble](https://www.rdocumentation.org/packages/tibble/versions/3.1.0)'
from the [tidyverse](https://www.tidyverse.org/) as inspiration for
functionality.

Data frames can contain values of any type. If desired, additional
attributes, such as the numerical type, unit and other information may be
attached to the variable for convenience or efficiency. For example
you could specify a unit of length, say m/s (meters per second), to
ensure that mathematical operations on that variable always produce
lengths (though the unit may change).

### DFIO {#dfio}

The Data Frame I/O system provides input and output operations for
data frames. A data frame may be written to and read from files,
strings or streams, including network streams or relational databases.

### Select {#select}

Select is a facility for selecting portions of sequences or arrays. It provides:

- An API for making selections (elements selected by the Cartesian
  product of vectors of subscripts for each axis) of array-like
  objects. The most important function is `select`.  Unless you want
  to define additional methods for `select`, this is pretty much all
  you need from this library.
- An extensible DSL for selecting a subset of valid subscripts.  This
  is useful if, for example, you want to resolve column names in a
  data frame in your implementation of select, or implementing
  filtering based on row values.

### Array Operations {#aops}

This library is a collection of functions and macros for manipulating
Common Lisp arrays and performing numerical calculations with
them. The library provides shorthand codes for frequently used
operations, displaced array functions, indexing, transformations,
generation, permutation and reduction of columns.  Array operations
may also be applied to data frames, and data frames may be converted
to/from arrays.

### Special Functions {#special-functions}

This library implements numerical [special
functions](https://en.wikipedia.org/wiki/Special_functions) in Common
Lisp with a focus on high accuracy double-float calculations.  These
functions are the basis for the statistical distributions functions,
e.g. gamma, beta, etc.

### Cephes

[Cephes.cl](https://github.com/Lisp-Stat/cephes.cl) is a CFFI wrapper
over the Cephes Math Library, a high quality C implementation of
statistical functions. We use this both for an accuracy check (Boost
uses these to check its accuracy too), and to fill in the gaps where
we don't yet have common lisp implementations of these functions.


<!--
### Distributions {#distributions}

A library for probability distributions and associated functions, with
an emphasis on accuracy and correctness.  It provides a consistent
interface to the distributions and computes:

- Probability density/mass functions (PDF) and log-pdf
- Cumulative density functions (CDF)
- quantiles
- random draws from distributions
- mean, variance of distributions
-->

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

This is the top level system that uses the other packages to create a
statistical computing environment.  It is also the location for the
'unified' interface, where the holes are plugged with third party
packages. For example
[cl-mathstats](https://github.com/gwkkwg/cl-mathstats) contains
functionality not yet in Lisp-Stat, however its architecture does not
lend itself well to incorporation via an ASDF `depends-on`, so as we
consolidate the libraries, missing functionality will be placed in the
Lisp-Stat system.  Eventually parts of `numerical-utilities`,
especially the statistics functions, will be relocated here.

## Acknowledgements {#acknowledgements}

Tamas Papp was the original author of many of these
libraries.  Starting with relatively clean, working, code that solves
real-world problems was a great start to the development of Lisp-Stat.


## What next?

{{< cardpane >}}
  {{< card header="**Get Started**" >}}
  [Install](/docs/getting-started/installation/)<br/>
  [Get Help](/docs/getting-started/getting-help/)<br/>
  [Plot in 5 minutes](/docs/getting-started/)<br/>
  {{< /card >}}
  {{< card header="**Examples**" >}}
  [Analytics](/docs/examples/statistics/)<br/>
  [Plotting](/docs/examples/plotting/)
  {{< /card >}}
  {{< card header="**R Users**" >}}
  [Basic tutorial](/docs/tutorials/basics/)<br/>
  [Data Frame](/docs/tutorials/data-frame/)
  {{< /card >}}
{{< /cardpane >}}

