---
title: "Exploratory Data Analysis"
linkTitle: "Analysis"
weight: 3
date: 2021-04-27
draft: true
description: >
  An exploratory data analysis of the R nycflights13 data set.
---

The
[nycflights13](https://github.com/tidyverse/nycflights13)
data set shows airline on-time data for all flights departing NYC in
2013. The [data
formats](https://rdrr.io/cran/nycflights13/man/flights.html) are
found in the R documentation.

## Prerequisites

To load the required systems:

```lisp
(ql:quickload :lisp-stat)
(ql:quickload :lisp-stat/rdata)
(ql:quickload :sqldf)
(in-package #:ls-user)
```

Now, load the `nycflights13` data set into a data frame:

```lisp
(define-data-frame flights
	(read-csv
		(rdata:rdata 'rdata:nycflights13 'rdata:flights)))
```

