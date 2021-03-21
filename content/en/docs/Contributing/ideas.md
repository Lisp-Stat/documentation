---
title: "Contribution Ideas"
linkTitle: "Ideas"
weight: 10
description: >
  Some ideas on how contribute to Lisp-Stat
---

## Small projects

There are three documentation pages that represent the 'to be' state:

- [Basic Tutorial](/docs/tutorials/basics/)
- [Working with Data](/docs/tasks/data-frame/)
- [Examples](/docs/examples/)

In order of increasing complexity:

In the case of the basic examples, we are following along with the
[Introduction to the Practice of Statistics in
R](https://nhorton.people.amherst.edu/ips9/).  If you'd like to
implement one of the examples in LISP-STAT, [open an
issue](https://github.com/lisp-stat/IPS/issues) in the IPS repository.

The basic tutorial was derived from the XLISP-STAT tutorial, and the
missing functionality is commmented out.  As an example, the tutorial
covers ANOVA; not yet in LISP-STAT, but available in cl-mathstats.
Updating a piece of the tutorial and implementing the missing
functionality is a nicely bounded task.

The data frame documentation and implementation is a bit more
open-ended. We don't wish to exactly follow the `tibble` API, but
require much of the same functionality as
[dplyr](https://dplyr.tidyverse.org/index.html) (as would any
statistics system).  This task is a bit more open-ended and would
require some experience in statistical analysis and group discussion,
but they can be picked off one-by-one.

## Special Functions

The functions underlying the statistical distributions require skills
in numerical programming. If you like being 'close to the metal', this
is a good area for contributions. Suitable for medium-advanced level
programmers. In particular we need implementations of:

- gamma
- incomplete gamma (upper & lower)
- inverse incomplete gamma

This work is partially complete and makes a good starting point for
someone who wants to make a substantial contribution.

## Documentation

Better and more documentation is always welcome, and a great way to
learn. Suitable for beginners to Common Lisp or statistics.

## Jupyter-Lab Integrations

Jupyter Lab has two nice integrations into Pandas, the Python version
of Data-Frame, that would make great contributions:
[Qgrid](https://github.com/quantopian/qgrid), which allows editing a
data frame in Jupyter Lab, and [Jupyter
DataTables](https://pypi.org/project/jupyter-datatables/). There are
many more Pandas/Jupyter integrations, and any of them would be
welcome additions to the Lisp-Stat ecosystem.

## Plotting

LISP-STAT has a basic plotting system, but there is always room for
improvement.  An interactive REPL based plotting system should be
possible with a medium amount of
effort. [Remote-js](https://github.com/ceramic/remote-js) provides a
working example of running JavaScript in a browser from a REPL, and
could combined with something like Electron and a DSL for VegaLite
specifications. This may be a 4-6 week project for someone with
JavaScript and HTML skills.  There are other Plotly/Vega options, so
if this interests you, open an issue and we can discuss. I have
working examples of much of this, but all fragmented examples. Skills:
good web/javascript, beginner lisp.

## Regression

We have some code for 'quick & dirty' regressions and need a more
robust DSL (Domain Specific Language). As a prototype, the -proto
regression objects from XLISP-STAT would be both useful and be a good
experiment to see what the final form should take. This is a
relatively straightforware port, e.g. `defproto -> defclass` and
`defmeth -> defmethod`. Skill level: medium in both Lisp and
statistics, or willing to learn.

## Vector Mathmatics

We have code for vectorised versions of all Common Lisp functions,
living in the `elmt` package. It now only works on vectors.  Shadowing
Common Lisp mathmatical operators is possible, and more natural.  This
task is to make `elmt` vectorised math functions work on lists as well
as vectors, and to implement shadowing of Common Lisp.  This task
requires at least medium-high level Lisp skills, since you will be
working with both packages and shadowing.  We also need to run the
ANSI Common Lisp conformance tests on the results to ensure there are
no dependencies we are unaware of.

## Continuous Integration

If you have experience with Github's CI tools, a CI setup for
Lisp-Stat would be a great help. This allows people making pull
requests to immediately know if their patches break anything. Beginner
level Lisp.
