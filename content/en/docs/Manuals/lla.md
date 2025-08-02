---
title: "Linear Algebra"
author: ["Steven Nunez"]
date: 2022-01-11
weight: 5
description: >
  Linear Algebra for Common Lisp
---

## Overview {#Overview}

LLA works with matrices, that is arrays of rank 2, with all numerical values.  Categorical variables could be integer coded if you need to.


## Setup

`lla` requires a BLAS and LAPACK shared library. These may be available via
your operating systems package manager, or you can download [OpenBLAS](https://github.com/xianyi/OpenBLAS), which includes precompiled binaries for MS Windows.

{{< alert title="Note" >}}LLA relies on
[CFFI](https://common-lisp.net/project/cffi/manual/cffi-manual.html)
to locate the BLAS & LAPPACK shared libraries. In most cases, this means CFFI
will use the system default search paths. If you encounter errors in
loading the library, consult the CFFI documentation. For MS Windows,
the certain way to successfully load the DLL is to ensure that the
library is on the `PATH`.{{</alert >}}

You can also configure the path by setting the `cl-user::*lla-configuration*` variable like so:
```lisp
(defvar cl-user::*lla-configuration*
  '(:libraries ("s:/src/lla/lib/libopenblas.dll")))
```
Use the location specific to your system.

To load `lla`:

```lisp
(asdf:load-system :lla)
(use-package 'lla) ;access to the symbols
```

### Getting started

To make working with matrices easier, we're going to use the matrix-shorthand library.  Load it like so:

```lisp
(use-package :num-utils.matrix-shorthand)
```

### Matrix Multiplication
`mm` is the matrix multiplication function.  It is generic and can operate on both regular arrays and 'wrapped' array types, e.g. hermitian or triangular.  In this example we'll multiple an array by a vector.  `mx` is the short-hand way of defining a matrix, and `vec` a vector.


```lisp
(let ((a (mx 'lla-double
           (1 2)
           (3 4)
           (5 6)))
      (b2 (vec 'lla-double 1 2)))
  (mm a b2))

; #(5.0d0 11.0d0 17.0d0)
```

## Numeric types
These are synonyms for types generally recognized by LLA.  Note that
LLA works fine with any kind of number, but may be faster for array
element types it recognizes as it does not have to spend time
detecting a common type.  Think of these types as optimization hints
similar to those used in `MAKE-ARRAY` et. al.

* lla-integer
* lla-single
* lla-double
* lla-complex-single
* lla-complex-double

These types are defined in the `lla` package.

## Matrix types

`num-utils.matrix` defines generic functions and types for commonly encountered matrices.  `LLA` imports this package.

### diagonal

### triangular

### hermitian





## Matrix shorthand
The matrix shorthand package, `num-utils.matrix-shorthand`, with nickname `nu.mx` defines short function names for creating commonly used matrix types.


### mx - create matrix

### vec - create vector

### diagonal-mx

### lower-triangular-mx

### upper-triangular-mx

### hermitian-mx

<!--
## Basics

### norm

**`norm`** returns a matrix or vector norm.  This function is able to return one of eight different matrix norms, or one of an infinite number of vector norms (described below), depending on the value of the `ord` parameter.  `ord` may be one of: `integer`, `:frob`, `:inf` `:-inf`. `ord` must be >= 0.

Note that norm is not, by default, part of the LS-USER package.

The following norms can be calculated:

| ord     | norm for matrices          | norm for vector |
|---------|----------------------------|-----------------|
| None    | Frobenious norm            | 2-norm          |
| `:frob` | Frobenius norm             | -               |
| `:nuc`  | nuclear norm               | -               |
| `:inf`  | (max(sum(abs(a), axis=1))) | (max (abs a))   |
| `:-inf` | (min(sum(abs(a), axis=1))) | (min (abs a))   |
| `0`     | -                          | (sum a)         |
| `1`     | (max(sum(abs(a), axis=0))) | as below        |
| `-1`    | N/A                        | as below        |
| `2`     | 2-norm                     | as below        |
| `-2`    | N/A                        | as below        |

The Frobenius norm is given by

The nuclear norm is the sum of the singular values.

Both the Frobenius and nuclear norm orders are only defined for matrices.

#### Examples

```lisp
(asdf:load-system :array-operations) ; if not using lisp-stat
(defparameter a #(-4 -3 -2 -1  0  1  2  3  4))
(defparameter b (aops:reshape a '(3 3)))
```

```lisp
LS-USER> (nu:norm a)
7.745967
LS-USER> (nu:norm b)
7.745967
LS-USER> (nu:norm b :frob)
7.745967
LS-USER> (nu:norm a :inf)
4
LS-USER> (nu:norm b :inf)
9
LS-USER> (nu:norm a :-inf)
0
LS-USER> (nu:norm b :-inf)
2
```

```lisp
LS-USER> (nu:norm a 1)
20
LS-USER> (nu:norm b 1)
7
LS-USER> (nu:norm a 2)
7.745967
LS-USER> (nu:norm b 2)
7.745967
LS-USER> (nu:norm b 3)
5.8480353
```
-->

