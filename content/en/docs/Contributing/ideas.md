---
title: "Contribution Ideas"
linkTitle: "Ideas"
weight: 13
description: >
  Some ideas on how contribute to Lisp-Stat
---

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

Jupyter Lab has two nice integrations with Pandas, the Python version
of Data-Frame, that would make great contributions:
[Qgrid](https://github.com/quantopian/qgrid), which allows editing a
data frame in Jupyter Lab, and [Jupyter
DataTables](https://pypi.org/project/jupyter-datatables/). There are
many more Pandas/Jupyter integrations, and any of them would be
welcome additions to the Lisp-Stat ecosystem.

## Regression

We have some code for 'quick & dirty' regressions and need a more
robust DSL (Domain Specific Language). As a prototype, the -proto
regression objects from XLISP-STAT would be both useful and be a good
experiment to see what the final form should take. This is a
relatively straightforward port, e.g. `defproto -> defclass` and
`defmeth -> defmethod`. Skill level: medium in both Lisp and
statistics, or willing to learn.

## Vector Mathematics

We have code for vectorized versions of all Common Lisp functions,
living in the `nu` package. It now only works on vectors.  Shadowing
Common Lisp mathematical operators is possible, and more natural.
This task is to shadow the Common Lisp mathmatical operators, e.g. `+`
so that they will work on atoms and vectors; essentially implement a
superset that is ANSI compatible.  This task requires at least
medium-high level Lisp skills, since you will be working with both
packages and shadowing.  We also need to run the ANSI Common Lisp
conformance tests on the results to ensure nothing gets broken in the
process.

## Continuous Integration

If you have experience with Github's CI tools, a CI setup for
Lisp-Stat would be a great help. This allows people making pull
requests to immediately know if their patches break anything. Beginner
level Lisp.

## Quarto

Integrate Lisp-Stat with [Quarto](https://quarto.org/). If Python, R
and Julia can integrate, so can Common Lisp.  We already have
[common-lisp-jupyter](https://github.com/yitzchak/common-lisp-jupyter)
which gets us pretty close.  It's mostly a Quarto configuration
exercise and modifying `plot/vega` to output PNG files.