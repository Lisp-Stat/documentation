---
title: "2023 End of Year Summary"
linkTitle: "2023 Summary"
date: 2023-12-29
description: >
  Year end wrap-up
---

We started the year by working through the [examples from the first chapter of the Introduction to the Practice of Statistics](https://github.com/Lisp-Stat/IPS9).  Whilst this is an excellent test for Lisp-Stat, Chapter 2 presented some major challanges to the plotting system, so the highlight for this year is Plot 2.0.  This is a breaking change, however the fix for plots is relatively simple.

The updates for 2023 include:

* [IPS Chapter 1](https://github.com/Lisp-Stat/IPS9/blob/master/notebooks/Part%20I/Chapter%201%20Looking%20at%20Data.ipynb) complete
* [IPS Chapter 2](https://github.com/Lisp-Stat/IPS9/blob/master/notebooks/Part%20I/Chapter%202%20Data%20Relationships.ipynb) plotting
* [Sampling](/docs/manuals/data-frame/#sampling) data frames
* Adopted [cl-gists](https://github.com/Symbolics/cl-gists) so we can work with the Vega eco-system on Github
* Added [Lisp Linear Algebra](https://github.com/Lisp-Stat/lla), CFFI wrappers for BLAS and LAPACK
* Improved tutorials, examples and documentation
* Plot 2.0
    * [Smoothers](https://github.com/Lisp-Stat/smoothers) for non-parametric regression
    * Plot helpers for qq-plots and scatterplots
    * New data sources: named data sets, URLs, embedded
    * Publish plots to Github gists
    * [Edit plots in Vega Edit online editor](/docs/tutorials/plotting/#vega-edit)
	* Fully support multi-layer, multi-data set plotting

