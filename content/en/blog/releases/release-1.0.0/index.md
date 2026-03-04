
---
title: "2022 Release"
linkTitle: "Release 1.0.0"
date: 2022-07-19
author: Steve Nunez
vega: true
description: >
  Lisp-Stat 1.0.0
---

## Overview

This release is the third major refactoring of Lisp-Stat, with
particular emphasis on plotting.  I have attempted to make plotting as
easy in Lisp-Stat as it is in
[Vega-Lite](https://vega.github.io/vega-lite/), upon which it is
based.  In addition, much clean-up and polishing of sharp edges has
taken place as a result of using Lisp-Stat on real-world projects.

As always, comments and contributions are welcome.


## What's New

### Lisp-Stat

- Data loading functions akin to R's
  [data](https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/data)
  function. You can now load the Lisp-Stat data sets with this
  function, e.g. `(data :mtcars)`.  The mechanism is extensible for
  add on packages to use as well.
- Additional cleansed and annotated data sets included.


### Plot

- The plotting system has been completely reworked. For example, to
create a scatter plot of horsepower vs. miles per gallon:

```lisp
(asdf:load-system :plot/vega)
(data :vgcars)
(plot:plot
  (vega:defplot hp-mpg
  `(:title "Horsepower vs. MPG"
    :data (:values ,vgcars)
    :mark :point
	:encoding (:x (:field vgcars:horsepower)
	           :y (:field vgcars:miles-per-gallon)))))
```

{{< vega id="blog-hp-mpg" spec="/plots/hp-mpg.vl.json" >}}

- 50+ new [plotting examples](/docs/examples/plotting/) for
  commonly used statistical plots, along with a [plotting
  tutorial](/docs/tutorials/plotting/).


### Data-frame

- Paid technical debt, refactored, made ready for future progress.
- Added `filter-rows`, `remove-columns`, `remove-columns!` and `rename-column!`.
- Improved unit tests.
- Added column types: `categorical` (factor) and `temporal`.
- Improved error handling, conditions and restarts.
- Working prototypes for `stack`, `split-apply-combine`, `sort` operations.


### Array Operations

Array operations has been rehomed and now lives in the Lisp-Stat
github organisation.  Significant improvements to
[documentation](/docs/manuals/array-operations/) over the previous
repository.

### Documentation

There probably isn't a part of the [documentation](/docs/) that hasn't been touched.
Improvements in all areas, including organisation, expanded topic
coverage and quality improvements.

### Semantic Versioning

Previously Lisp-Stat used semantic versioning appropiate for binary
patch distribution.  In this scheme the major number indicates whether
or not the lisp image needed to be cold started, and a minor version
indicating if a patch could be applied.  Since it's unlikely we'll
have a viable patching system in the open source Common Lisp world
anytime soon, we've moved to [semantic versioning for source
releases](https://semver.org/). Since the semantics of versioning are
different, all systems have been reset to 1.0.0.  Going forward we'll
use the _major_._minor_._patch_ scheme.
