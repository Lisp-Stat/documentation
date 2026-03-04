---
title: "Statistics"
author: ["Steven Nunez"]
date: 2024-02-17
weight: 8
description: >
  Statistical functions
---

## Overview {#Overview}

`statistics` is a library that consolidates three well-known statistical libraries:

- The statistics library from numerical-utilities
- Larry Hunter's cl-statistics
- Gary Warren King's cl-mathstats

There are a few challenges in using these as independent systems on projects though:

- There is a good amount of overlap.  Everyone implements, for example `mean` (as does alexandria, cephes, and almost every other library out there).
- In the case of `mean`, `variance`, etc., the functions deal only with samples, not distributions

This library brings these three systems under a single 'umbrella', and adds a few missing ones.  To do this we use Tim Bradshaw's [conduit-packages](https://github.com/tfeb/conduit-packages).  For the few functions that require dispatch on type (sample data vs. a distribution), we use `typecase` because of its simplicity and not needing another system.  There's a slight performance hit here in the case of run-time determination of types, but until it's a problem prefer it.  Some alternatives considered for dispatch was https://github.com/pcostanza/filtered-functions.

### nu-statistics
These functions cover sample moments in detail, and are accurate.  They include up to forth moments, and are well suited to the work of an econometrist (and were written by one).

### lh-statistics
These were written by Larry Hunter, based on the methods described in Bernard Rosner's book, *Fundamentals of Biostatistics* 5th Edition, along with some from the [CLASP](https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/math/clasp/0.html) system.  They cover a wide range of statistical applications.  Note that `lh-statistics` uses lists and not vectors, so you'll need to convert.  To see what's available see the [statistics github repo](https://github.com/Lisp-Stat/statistics).

### gwk-statistics
These are from Gary Warren King, and also partially based on CLASP.  It is well written, and the functions have excellent documentation.  The major reason we don't include it by default is because it uses an older ecosystem of libraries that duplicate more widely used system (for example, numerical utilities, alexandria).  If you want to use these, you'll need to uncomment the appropriate code in the ASDF and `pkgdcl.lisp` files.

### ls-statistics
These are considered the most complete, and they account for various types and dispatch properly.

### Accuracy
LH and GWK statistics compute quantiles, CDF, PDF, etc. using routines from CLASP, that in turn are based on algorithms from Numerical Recipes.  These are known to be accurate to only about four decimal places.   This is probably accurate enough for many statistical problems, however should you need greater accuracy look at the [distributions](https://github.com/Lisp-Stat/distributions) system.  The computations there are based on [special-functions](https://github.com/Lisp-Stat/special-functions), which has accuracy around 15 digits.  Unfortunately documentation of distributions and the 'wrapping' of them here are incomplete, so you'll need to know the pattern, e.g. pdf-gamma, cdf-gamma, etc., which is described in the link above.

### Versions
Because this system is likely to change rapidly, we have adopted a system of versioning proposed in [defpackage+](https://github.com/rpav/defpackage-plus#versioning).  This is also the system `alexandria` uses where a version number is appended to the API.  So, `statistics-1` is our current package name.  `statistics-2` will be the next and so on.  If you don't like these names, you can always change it locally using a [package local nickname](https://lispcookbook.github.io/cl-cookbook/packages.html#package-local-nicknames-pln).

## Dictionary

### scale
**`scale`** scale is generic function whose default method centers and/or scales the columns of a numeric matrix.  This is neccessary when the units of measurement for your data differ.  The `scale` function is provided for this purpose.

```lisp
(defun standard-scale (x &key center scale)
```

#### Returns
The function returns three values:

1. (x - x̄) / s where X̄ is the mean and S is the standard deviation
2. the `center` value used
3. the `scale` value used

#### Parameters

- `CENTRE` value to center on.  `(mean x)` by default
- `SCALE` value to scale by.  `(sd x)` by default

If `center` or `scale` is `nil`, do not scale or center respectively.

#### Example: Scale the values in a vector

```lisp
(defparameter x #(11 12 13 24 25 16 17 18 19))
(scale x)
; => #(-1.2585064099313854d0
       -1.0562464511924128d0
	   -0.85398649245344d0
	    1.3708730536752591d0
	    1.5731330124142318d0
	   -0.24720661623652215d0
	   -0.044946657497549475d0
	    0.15731330124142318d0
	    0.3595732599803958d0)
```

Note that the scaled vector contains negative values.  This is expected scaling a vector.  Let's try the same thing but without scaling:

```lisp
(scale x :scale nil)
#(-56/9 -47/9 -38/9 61/9 70/9 -11/9 -2/9 7/9 16/9)
155/9
1
```

Note the scaling factor was set to 1, meaning no scaling was performed, only centering (division by zero returns the original value).

#### Example: Scale the columns of an array

```lisp
(defparameter y #2A((1 2 3 4 5 6 7 8 9)
		    (10 20 30 40 50 60 70 80 90)))
(margin #'scale y 1) ; 1 splits along columns, 0 splits along rows
#(#(-1.4605934866804429d0 -1.0954451150103321d0 -0.7302967433402214d0 -0.3651483716701107d0 0.0d0 0.3651483716701107d0 0.7302967433402214d0 1.0954451150103321d0 1.4605934866804429d0)
  #(-1.4605934866804429d0 -1.0954451150103321d0 -0.7302967433402214d0 -0.3651483716701107d0 0.0d0 0.3651483716701107d0 0.7302967433402214d0 1.0954451150103321d0 1.4605934866804429d0))
```

#### Example: Scale the variables of a data frame

```lisp
LS-USER> (remove-column! iris 'species) ;species is a categorical variable
#<DATA-FRAME (150 observations of 4 variables)
Edgar Anderson's Iris Data>
LS-USER> (head iris)

;;   SEPAL-LENGTH SEPAL-WIDTH PETAL-LENGTH PETAL-WIDTH
;; 0          5.1         3.5          1.4         0.2
;; 1          4.9         3.0          1.4         0.2
;; 2          4.7         3.2          1.3         0.2
;; 3          4.6         3.1          1.5         0.2
;; 4          5.0         3.6          1.4         0.2
;; 5          5.4         3.9          1.7         0.4
NIL
LS-USER> (map-columns iris #'scale)
#<DATA-FRAME (150 observations of 4 variables)>
LS-USER> (head *)

;;          SEPAL-LENGTH          SEPAL-WIDTH        PETAL-LENGTH         PETAL-WIDTH
;; 0             -0.8977               1.0156             -1.3358             -1.3111
;; 1             -1.1392              -0.1315             -1.3358             -1.3111
;; 2             -1.3807               0.3273             -1.3924             -1.3111
;; 3             -1.5015               0.0979             -1.2791             -1.3111
;; 4             -1.0184               1.2450             -1.3358             -1.3111
;; 5             -0.5354               1.9333             -1.1658             -1.0487
```




