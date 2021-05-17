---
date: 2021-05-09
title: "Statistical Analysis with Lisp-Stat"
linkTitle: "Announcing Lisp-Stat"
description: "Version 1.0-beta"
author: Steve Nunez
resources:
- src: "**.{png,jpg}"
  title: "Image #:counter"
---

Lisp-Stat is the culmination of many months work to pull together the
best-in-class statistical analysis packages available in Common Lisp,
under a commercially friendly license, usable ‘out of the box’.  It is
an environment for statistical computing, conceptually similar to R,
that is also suitable for front-line production deployments.  It grew
out of a desire to have an environment for rapidly prototyping
analytical and A.I. solutions, and move directly to production
environments with minimal friction.  Typically there is a gap between
the modeling environment, perhaps R or Python, and the delivery
environment. Lisp-Stat closes that gap.

Lisp-Stat is in daily use for statistical analysis and is now
considered ready for a broader audience.


## Why Lisp?
We had a few requirements when evaluating options.  Specifically the
system had to:

- Work well in the kind of exploratory environment conducive to analytics and AI
- Be robust enough to be deployed in an enterprise-level production environment
- Be available under a license without source code restrictions

Common Lisp was the only framework that met all these requirements.
And, it is not the first time lisp has been used in a statistical
setting.  [XLISP-STAT](https://en.wikipedia.org/wiki/XLispStat), our
spiritual predecessor, was a contemporary of R in the early days of
development.  Wikipedia says about it: "XLispStat was historically
influential in the field of statistical visualization" and its author,
Luke Tierney, was a member of the original R core team.


## What does Lisp-Stat do?

Lisp-Stat provides support for vectorized mathematical operations, a
super-set of common lisp [array
operations](/docs/tasks/array-operations/), and a comprehensive set of
statistical methods that are implemented using the latest numerical
algorithms.  In addition, Common Lisp provides a dynamic programming
environment that includes a
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
for interactive evaluation of code,
[CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System), an
excellent object oriented environment and a meta-object protocol
([MOP](https://en.wikipedia.org/wiki/Metaobject#Metaobject_protocol)).

We have also compiled an archive of [XLISP-STAT
libraries](/docs/reference/xlisp/) that can be ported with the aid of
a compatibility package (XLS).  This gives Lisp-Stat a leg up on
ecosystem development; there are many useful statistical routines in
the archive.

## Getting Started

If you are familiar with R, and want to understand how to do similar
things with Lisp-Stat, the [basic tutorial](/docs/tutorials/basics) is
the place to begin.  If you have a Common Lisp installation and want
give Lisp-Stat a quick try, start with [Install to Plot in 5
minutes](/docs/getting-started/).  For newcomers to lisp, see the
[Installation](/docs/getting-started/installation/) section for how to
get going from ground zero.

## What's in the box?

All the basics for descriptive statistical analysis.

### Data Frames
Load the classic R `mtcars` data set into a data frame:

```lisp
(define-data-frame mtcars
  (read-csv (rdata 'datasets 'mtcars)))
```

### Plotting

The [plotting examples](/docs/examples/plotting/) show how to plot the
horsepower vs. miles-per-gallon of the similar Vega-Lite data set:

```
(defparameter cars-scatter-plot
  (vglt:scatter-plot vega-cars "HORSEPOWER" "MILES_PER_GALLON"))
(plot:plot-from-file (vglt:save-plot 'cars-scatter-plot))
```

{{< figure src="/docs/examples/vega-cars-colored-scatter-plot.png" >}}

### Jupyter notebooks
For reproducible research and sharing.

{{< imgproc jupyter-notebook Fill "600x600" >}}
Lisp-Stat running in a Jupyter-Lab notebook
{{< /imgproc >}}

### Examples & Tutorials

The [examples](/docs/examples/) show how to create [common
plots](/docs/examples/plotting/) and the [analytic
examples](/docs/examples/analysis/) work through the examples of the
book _Introduction to the Practice of Statistics_ using Lisp-Stat.

### Example data sets

About 1500 [R data sets](/docs/tasks/data-frame/#example-datasets) can be
loaded using the `rdata` package.

### Where do I get help?

The [community page](/community) page describes how the project is
structured.  Help can be obtained from:

- [Lisp-Stat mailing list](https://groups.google.com/g/lisp-stat)
- [Stack overflow](https://stackoverflow.com/questions/tagged/xlispstat)
- [Github discussion](https://github.com/Lisp-Stat/lisp-stat/discussions)

## Where did it come from?

Several of these libraries are based on the work of [Tamas K
Papp](https://tamaspapp.eu/), who was working as a practicing
econometrician using Common Lisp before leaving the community for
Julia.  One of the design goals for Lisp-Stat was to avoid the [Lisp
Curse](http://www.winestockwebdesign.com/Essays/Lisp_Curse.html) that
plagues so many common lisp niches, including statistics.  So, rather
than reinvent the wheel, we took some existing wheels, greased and
polished them, added documentation and filed off the rough edges to
make them work smoothly together in an integrated environment.  The
greenfield work includes `special functions` and `plot`.

## Haven't I seen this before?

You may have. In late March 2021 existence of Lisp-Stat was leaked to
Hacker News and Reddit.  At that time, we were still getting Lisp-Stat
ready for general release, and things were not quite where we wanted
them to be.  If you saw the original leaked version, here are the
changes:

- Documentation:
  - Reorganized getting started, tutorials, reference
  - Added plotting and data-frame examples
  - Added "Install to plot in 5 minutes" getting started guide
  - Expanded analytic examples
  - Expanded basic tutorial
  - QA: spelling, grammar, typos, examples
  - Added resources section
  - Added search
- Data Frame
  - Added pretty printing
  - Added markdown printing
  - Added variable shortcuts, you can now refer to a variable by name in all contexts
  - Added head, tail, summary, column-name functions
  - Added define-data-frame macro
  - Miscellaneous bug fixes
- Plot
  - Added box plot
  - Added histogram




